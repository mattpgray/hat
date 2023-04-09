#!/usr/bin/env python3

import argparse
import json
import os
import subprocess
import sys


script_dir = os.path.dirname(os.path.abspath(__file__))
def local_path(*args):
    return os.path.abspath(os.path.join(script_dir, "..", *args))


def test_files():
    example_dir = local_path("examples")
    for f in os.listdir(example_dir):
        path = os.path.join(example_dir, f)
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
    com_result = subprocess.run(["./target/release/hat", "com", file], capture_output=True)
    if com_result.returncode != 0:
        return com_result
    return subprocess.run(["./out"], capture_output=True)

def run_sim(file):
    return subprocess.run(["./target/release/hat", "sim", file], capture_output=True)

def run_tests(args, runner):
    result = 0
    for file in test_files():
        if args.update:
            print(f"Updating test for {file}")
        else:
            print(f"Running test for {file}")
        proc_result = runner(file)
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

    parser = argparse.ArgumentParser(
        prog = 'hat tester', 
        description = 'Tests the hat compiler against a set of example programs'
    )

    parser.add_argument('-u', '--update', action="store_true", help="Update the test outputs instead of testing")
    parser.add_argument('-m', '--mode', default="com", help="The mode of hat to test.", choices=("sim", "com"))
    args = parser.parse_args()

    print("Compling hat...")
    subprocess.run(["cargo", "build", "--release"])

    if args.mode == "sim":
        result = run_tests(args, run_sim)
    elif args.mode == "com":
        result = run_tests(args, run_com)
    else:
        print(f"Invalid mode {args.mode}")
        result = 1

    exit(result)

if __name__ == "__main__":
    main()

