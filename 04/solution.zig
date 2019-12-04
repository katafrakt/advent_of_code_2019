const assert = @import("std").debug.assert;
const warn = @import("std").debug.warn;

fn numToDigits(num: i32) [6]i32 {
    var array: [6]i32 = undefined;
    var value: i32 = num;
    
    for (array) |*item| {
        const digit = @mod(value, 10);
        item.* = digit;
        value = @divFloor(value, 10);
    }
    return array;
}

test "numToDigits" {
    const num: i32 = 321456;
    const re = numToDigits(num);
    assert(re[0] == 6);
    assert(re[3] == 1);
}

fn isOk(digits: [6]i32) bool {
    var adjacent = false;
    var nonIncreasing = true; // since we're going opposite direction, condition changes
    var lastDigit = digits[0];
    var idx = @intCast(i32, 1);
    while(idx < 6) {
        var currentDigit = digits[@intCast(usize, idx)];
        if (currentDigit > lastDigit) {
            nonIncreasing = false;
        }
        if (currentDigit == lastDigit) {
            adjacent = true;
        }

        idx += 1;
        lastDigit = currentDigit;
    }

    return (adjacent and nonIncreasing);
}

test "isOK" {
    assert(isOk(numToDigits(111111)));
    assert(!isOk(numToDigits(223450)));
    assert(!isOk(numToDigits(123789)));
}

fn hasAdjacent2(digits: [6]i32) bool {
    var lastDigit = digits[1];
    var lastButOneDigit = digits[0];
    var idx = @intCast(i32, 2);
    var adjacent2 = lastDigit == lastButOneDigit;

    while (idx < 6) {
        var currentDigit = digits[@intCast(usize, idx)];
        if (adjacent2 and currentDigit != lastDigit) {
            return true;
        } else if (adjacent2) {
            adjacent2 = false;
        } else {
            adjacent2 = currentDigit == lastDigit and currentDigit != lastButOneDigit;
        }
        idx += 1;
        lastButOneDigit = lastDigit;
        lastDigit = currentDigit;
    }

    return adjacent2;
}

test "hasAdjacent2" {
    assert(hasAdjacent2(numToDigits(112233)));
    assert(!hasAdjacent2(numToDigits(123444)));
    assert(!hasAdjacent2(numToDigits(444321)));
    assert(!hasAdjacent2(numToDigits(444444)));
    assert(hasAdjacent2(numToDigits(444322)));
    assert(hasAdjacent2(numToDigits(443222)));
    assert(hasAdjacent2(numToDigits(111122)));
    assert(hasAdjacent2(numToDigits(112222)));
}

pub fn main() void {
    var current: i32 = 278384;
    const limit: i32 = 824795;
    var count = @intCast(i32, 0);
    var count2 = @intCast(i32, 0);

    while(current <= limit) {
        if (isOk(numToDigits(current))) {
            count += 1;
            if(hasAdjacent2(numToDigits(current))) count2 += 1;
        }
        current += 1;
    }

    warn("{}\n", count);
    warn("{}\n", count2);
}