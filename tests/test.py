#!/usr/bin/env python3

import argparse
import json
import os
import subprocess
import sys
import shlex


def test_files():
    test_dir = "./tests"
    for f in os.listdir(test_dir):
        path = os.path.join(test_dir, f)
        if not os.path.isfile(path):
            continue
        _, ext = os.path.splitext(path)
        if ext != ".hat":
            continue
        yield path


def dict_compare(d1, d2):
    d1_keys = set(d1.keys())
    d2_keys = set(d2.keys())
    shared_keys = d1_keys.intersection(d2_keys)
    added = d1_keys - d2_keys
    removed = d2_keys - d1_keys
    modified = {o : (d1[o], d2[o]) for o in shared_keys if d1[o] != d2[o]}
    same = set(o for o in shared_keys if d1[o] == d2[o])
    return added, removed, modified, same

def run_com(file):
    output_file = "./testbin"
    com_result = run_cmd(["./target/release/hat", "com", file, "-o", output_file], capture_output=True)
    if com_result.returncode != 0:
        return com_result
    res = run_cmd([output_file], capture_output=True)
    os.remove(output_file)
    os.remove(f"{output_file}.o")
    os.remove(f"{output_file}.asm")
    return res

def run_cmd(cmd_args, *args, **kwargs):
    print("[cmd]", " ".join(shlex.quote(arg) for arg in cmd_args))
    return subprocess.run(cmd_args, *args, **kwargs)

def run_tests(args):
    result = 0
    for file in test_files():
        if args.update:
            print(f"Updating test for {file}")
        else:
            print(f"Running test for {file}")
        proc_result = run_com(file)
        result_dict = {
                "stdout": proc_result.stdout.decode("utf-8"),
                "stderr": proc_result.stderr.decode("utf-8"),
                "return_code": proc_result.returncode,
        }
        name, _ = os.path.splitext(file)
        name += ".json"
        if args.update:
            with open(name, "w") as f:
                json.dump(result_dict, f, indent=4, sort_keys=True)
        else:
            with open(name, "r") as f:
                want = json.load(f)

            added, removed, modified, same = dict_compare(result_dict, want)
            if len(added) != 0 or len(removed) != 0 or len(modified) != 0:
                print(f"{file} test failure {result_dict} != {want}", file=sys.stderr)
                result = 1
    return result

def main() :
    """
    This script allows for testing the hat programming language.
    It runs the program files in the examples directory and checks that
    the output has not changed.
    """

    # Nicer error message by changing directory. It means we do not get absolute paths in the error 
    # messages.
    script_dir = os.path.dirname(os.path.abspath(__file__))
    os.chdir(os.path.join(script_dir, ".."))


    parser = argparse.ArgumentParser(
        prog = 'hat tester', 
        description = 'Tests the hat compiler against a set of example programs'
    )

    parser.add_argument('-u', '--update', action="store_true", help="Update the test outputs instead of testing")
    args = parser.parse_args()

    run_cmd(["cargo", "build", "--release"], check=True)
    result = run_tests(args)

    exit(result)

if __name__ == "__main__":
    main()

