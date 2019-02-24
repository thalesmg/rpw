#!/usr/bin/env python3

import os
import sys
import subprocess
import termios

BASE_PATH = "./tests/"

def find_tests():
    output = set()
    for t in os.listdir(BASE_PATH):
        t = os.path.join(BASE_PATH, t)
        if os.path.isfile(t) and os.access(t, os.X_OK):
            output.add(t)
    return output

def run_retrying(test_path, fuel=4, old_attrs=None):
    print("{}... ".format(test_path), end="")
    last_failure = None
    for _ in range(fuel):
        res = subprocess.run([test_path], check=False, stdout=subprocess.PIPE)
        if old_attrs:
            termios.tcsetattr(sys.stdin, termios.TCSANOW, old_attrs)
        if res.returncode == 0:
            print("OK")
            return False
        else:
            last_failure = res.stdout.decode("utf-8")
    print("ERROR")
    if last_failure:
        print(last_failure)
    return True

if __name__ == "__main__":
    old_attrs = termios.tcgetattr(sys.stdin) if os.isatty(0) else None
    failure = False
    blacklist = {
        # "./tests/cat_break.exp"
    }
    for t in find_tests().difference(blacklist):
        result = run_retrying(t, old_attrs=old_attrs)
        failure = failure or result
    if failure:
        print("ERRORS!")
        exit(1)
    else:
        print("OK!")
        exit(0)
