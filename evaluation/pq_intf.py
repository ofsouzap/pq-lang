import subprocess


def generate_pq_source(
    mrd: int = 10,
    max_variant_types: int = 5,
    max_variant_type_constructors: int = 5,
    max_top_level_defns: int = 5,
):
    cmd = [
        "opam",
        "exec",
        "--",
        "dune",
        "exec",
        "pq_eval_tools",
        "--",
        "gen-program",
        "-r",
        str(mrd),
        "-t",
        str(max_variant_types),
        "-c",
        str(max_variant_type_constructors),
        "-d",
        str(max_top_level_defns),
    ]

    result = subprocess.run(cmd, capture_output=True, text=True, check=True)
    return result.stdout.strip()


def exec_pq_string(program_str: str):
    cmd = ["opam", "exec", "--", "dune", "exec", "pq_exec"]

    result = subprocess.run(
        cmd, input=program_str, capture_output=True, text=True, check=True
    )
    return result.stdout.strip()
