const std = @import("std");

pub fn build(b: *std.Build) void {
    const target = b.standardTargetOptions(.{});
    const optimize = b.standardOptimizeOption(.{});
    const omit_frame_pointer =
        if (target.result.cpu.arch == .x86_64) true else null;

    const core = b.createModule(.{
        .target = target,
        .optimize = optimize,
        .omit_frame_pointer = omit_frame_pointer,
        .link_libc = true,
        .root_source_file = b.path("src/runtime.zig"),
    });

    const lib = b.addLibrary(.{
        .name = "xi_runtime",
        .linkage = .static,
        .root_module = core,
    });

    lib.root_module.addIncludePath(b.path("vendor/stb"));
    lib.root_module.addCSourceFile(.{
        .file = b.path("csrc/stb_image_impl.c"),
        .flags = &.{"-std=c99"},
    });

    lib.link_function_sections = true;
    lib.link_data_sections = true;

    for ([_][]const u8{
        "time",
        "os",
        "math",
        "image",
    }) |area| {
        const mod = b.createModule(.{
            .target = target,
            .optimize = optimize,
            .omit_frame_pointer = omit_frame_pointer,
            .link_libc = true,
            .root_source_file = b.path(b.fmt("src/{s}.zig", .{area})),
        });
        const obj = b.addObject(.{ .name = b.fmt("xi_{s}", .{area}), .root_module = mod });
        obj.link_function_sections = true;
        obj.link_data_sections = true;
        lib.root_module.addObject(obj);
    }

    b.installArtifact(lib);
}
