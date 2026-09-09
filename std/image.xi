// Xi standard library: image decoding.
//
// Decoded images are normalized to tightly packed, top-to-bottom RGBA8 pixels.
// `load` reads from a path; `decode` accepts an already-loaded encoded image.

#include io
#include arr
#include mem

struct Image {
    int width;
    int height;
    int channels;
    array<uint8> pixels;
    bool valid;
    string error;
}

extern f _decode(array<uint8> encoded) -> array<uint8> = "xi_image_decode";
extern f _last_error() -> string = "xi_image_last_error";

f decode(array<uint8> encoded) {
    array<uint8> packet = image::_decode(encoded);
    if (arr::len(packet) < 12) {
        return Image {
            width: 0,
            height: 0,
            channels: 0,
            pixels: [],
            valid: false,
            error: image::_last_error()
        };
    }

    int width = mem::read_le(packet, 0, 4);
    int height = mem::read_le(packet, 4, 4);
    int channels = mem::read_le(packet, 8, 4);
    array<uint8> pixels = arr::slice_from(packet, 12);
    bool valid = width > 0 && height > 0 && channels == 4
        && arr::len(pixels) == width * height * channels;
    string error = "";
    if (!valid) {
        error = "decoded image packet was invalid";
    }
    return Image {
        width: width,
        height: height,
        channels: channels,
        pixels: pixels,
        valid: valid,
        error: error
    };
}

f load(string path) {
    return image::decode(io::read_bytes(path));
}
