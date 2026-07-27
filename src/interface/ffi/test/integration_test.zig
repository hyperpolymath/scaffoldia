// SPDX-License-Identifier: MPL-2.0
// Copyright (c) Jonathan D.A. Jewell <j.d.a.jewell@open.ac.uk>
// Scaffoldia FFI Integration Tests
//
// These tests verify that the Zig FFI correctly implements the Idris2 ABI
// (src/interface/Abi/Foreign.idr). They import the main FFI module by its
// registered build.zig module name ("scaffoldia") and exercise the exported
// functions directly, rather than only via the raw C ABI.

const std = @import("std");
const scaffoldia = @import("scaffoldia");

test "lifecycle: create and destroy handle" {
    const handle = scaffoldia.scaffoldia_init() orelse return error.InitFailed;
    defer scaffoldia.scaffoldia_free(handle);

    try std.testing.expect(scaffoldia.scaffoldia_is_initialized(handle) == 1);
}

test "lifecycle: free on a null handle is a safe no-op" {
    // scaffoldia_free(null) must not crash — the implementation returns
    // early via `handle orelse return`. Note: freeing the SAME live handle
    // twice is NOT safe (it would double-free through allocator.destroy),
    // so that is deliberately not exercised here.
    scaffoldia.scaffoldia_free(null);
}

test "operations: process with valid handle" {
    const handle = scaffoldia.scaffoldia_init() orelse return error.InitFailed;
    defer scaffoldia.scaffoldia_free(handle);

    const result = scaffoldia.scaffoldia_process(handle, 42);
    try std.testing.expectEqual(scaffoldia.Result.ok, result);
}

test "operations: process_array with valid buffer" {
    const handle = scaffoldia.scaffoldia_init() orelse return error.InitFailed;
    defer scaffoldia.scaffoldia_free(handle);

    const buf = [_]u8{ 1, 2, 3, 4 };
    const result = scaffoldia.scaffoldia_process_array(handle, &buf, buf.len);
    try std.testing.expectEqual(scaffoldia.Result.ok, result);
}

test "operations: process_array with null buffer returns null_pointer" {
    const handle = scaffoldia.scaffoldia_init() orelse return error.InitFailed;
    defer scaffoldia.scaffoldia_free(handle);

    const result = scaffoldia.scaffoldia_process_array(handle, null, 0);
    try std.testing.expectEqual(scaffoldia.Result.null_pointer, result);
}

test "strings: get_string returns a value that can be freed" {
    const handle = scaffoldia.scaffoldia_init() orelse return error.InitFailed;
    defer scaffoldia.scaffoldia_free(handle);

    const str = scaffoldia.scaffoldia_get_string(handle);
    defer if (str) |s| scaffoldia.scaffoldia_free_string(s);

    try std.testing.expect(str != null);
    if (str) |s| {
        const slice = std.mem.span(s);
        try std.testing.expect(slice.len > 0);
    }
}

test "version: returns non-empty version string" {
    const ver = scaffoldia.scaffoldia_version();
    const ver_str = std.mem.span(ver);
    try std.testing.expect(ver_str.len > 0);
}

test "build_info: returns non-empty build info string" {
    const info = scaffoldia.scaffoldia_build_info();
    const info_str = std.mem.span(info);
    try std.testing.expect(info_str.len > 0);
}

test "error handling: null handle on process returns null_pointer and sets last_error" {
    const result = scaffoldia.scaffoldia_process(null, 0);
    try std.testing.expectEqual(scaffoldia.Result.null_pointer, result);

    const err = scaffoldia.scaffoldia_last_error();
    try std.testing.expect(err != null);
}

test "error handling: is_initialized on null handle returns 0" {
    try std.testing.expect(scaffoldia.scaffoldia_is_initialized(null) == 0);
}
