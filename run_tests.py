import subprocess
import os

TEST_DIR = "tests"
TAP_EXECUTABLE = "./tap.native"

def compile_and_run(tap_file):
    base_name = os.path.splitext(os.path.basename(tap_file))[0]
    llvm_file = f"{base_name}.out"

    compile_cmd = [TAP_EXECUTABLE, "-l", tap_file]
    with open(llvm_file, "w") as f:
        subprocess.run(compile_cmd, stdout=f, stderr=subprocess.PIPE, check=True)

    result = subprocess.run(["lli", llvm_file], capture_output=True, text=True)
    return result.stdout.strip()

def run_tests():
    passed, total = 0, 0
    for file in os.listdir(TEST_DIR):
        if file.endswith(".tap"):
            total += 1
            tap_file = os.path.join(TEST_DIR, file)
            expected_file = os.path.join(TEST_DIR, file.replace(".tap", ".expected"))

            try:
                output = compile_and_run(tap_file)
                with open(expected_file, "r") as f:
                    expected_output = f.read().strip()
                if output == expected_output:
                    print(f"[PASS] {file}")
                    passed += 1
                else:
                    print(f"[FAIL] {file}")
                    print(f"Expected:\n{expected_output}\nGot:\n{output}")
            except subprocess.CalledProcessError as e:
                print(f"[ERROR] {file} - Compilation or Execution Failed")
                print(e)

    print(f"\n{passed}/{total} tests passed.")

if __name__ == "__main__":
    run_tests()