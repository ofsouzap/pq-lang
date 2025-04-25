from typing import List, Tuple, Sequence
from abc import ABC, abstractmethod
import random
from pathlib import Path
import matplotlib.pyplot as plt

from eval_lib.pq_intf import exec_pq_string
from eval_lib.investigation import (
    load_or_gen_results,
)

DATA_CACHE_PATH: Path = (
    Path(__file__).parent / Path("outputs") / Path("increasing_pattern_nesting.dat")
)
PLOT_OUTPUT_PATH: Path = (
    Path(__file__).parent
    / Path("outputs")
    / Path("eval-increasing_pattern_nesting.png")
)

_fresh_varname_counter: int = 0


def fresh_varname() -> str:
    global _fresh_varname_counter
    _fresh_varname_counter += 1
    return f"x{_fresh_varname_counter}"


class VType(ABC):
    @abstractmethod
    def gen_value(self) -> str:
        pass

    @abstractmethod
    def __str__(self) -> str:
        pass


class VTypeUnit(VType):
    def gen_value(self) -> str:
        return "()"

    def __str__(self) -> str:
        return "unit"


class VTypeInt(VType):
    def gen_value(self) -> str:
        return str(random.randint(-50, 50))

    def __str__(self) -> str:
        return "int"

    def nesting_level(self) -> int:
        return 0


class VTypePair(VType):
    def __init__(self, t1: VType, t2: VType):
        self.t1 = t1
        self.t2 = t2

    def gen_value(self) -> str:
        return f"({self.t1.gen_value()}, {self.t2.gen_value()})"

    def __str__(self) -> str:
        return f"(({self.t1}), ({self.t2}))"


class VTypeIntList(VType):
    def gen_value(self) -> str:
        if random.randint(0, 100) < 80:
            return "Nil ()"
        else:
            return f"Cons (({VTypeInt().gen_value()}), ({self.gen_value()}))"

    def __str__(self) -> str:
        return "int_list"

    def gen_matching_pattern(self, mrd: int) -> str:
        if mrd <= 0:
            return f"({fresh_varname()} : {self})"
        else:
            return random.choice(
                [
                    f"({fresh_varname()} : {self})",
                    f"Nil ({fresh_varname()} : unit)",
                    f"Cons (({fresh_varname()} : int), ({self.gen_matching_pattern(mrd - 1)}))",
                ]
            )


def gen_source(nesting_level: int) -> str:
    outs: list[str] = []

    outs.append("type int_list = Nil of unit | Cons of (int * int_list) ")

    t = VTypeIntList()
    arg = t.gen_value()

    case_count: int = random.randint(5 * nesting_level, 50 * nesting_level)

    cases: List[Tuple[str, str]] = [
        (t.gen_matching_pattern(nesting_level), str(i)) for i in range(case_count)
    ] + [(f"({fresh_varname()} : {t})", str(case_count))]

    outs.append(f"match ({arg}) -> int with")
    for case_p, case_e in cases:
        outs.append(f"  | ({case_p}) -> ({case_e})")
    outs.append(f"end")

    return "\n".join(outs)


def main():
    nesting_levels: list[int] = [5, 25, 45, 65, 85]

    all_results = load_or_gen_results(
        nesting_levels,
        inp_gen=gen_source,
        exec=exec_pq_string,
        data_cache_path=DATA_CACHE_PATH,
        n_trials=10,
        n_repeats=30,
    )
    all_results_values = sorted(all_results.values.values(), key=lambda x: x.seed)

    # Create the plot

    plot_vals_nesting_level = [res.seed for res in all_results_values]
    plot_vals_avg = [res.avg_time for res in all_results_values]
    plot_vals_std = [res.std for res in all_results_values]

    fig, ax = plt.subplots()

    # Plot with error bars

    ax.errorbar(
        plot_vals_nesting_level,
        plot_vals_avg,
        yerr=plot_vals_std,
        fmt="x-",
        color="tab:blue",
        ecolor="tab:gray",
        elinewidth=1,
        capsize=3,
    )

    ax.set_xlabel("Pattern Nesting Level")
    ax.set_ylabel("Time taken to check (s)")

    ax.grid()

    fig.suptitle(
        "Time taken to check for increasing pattern nesting level for flattening"
    )

    # Save the plot
    fig.savefig(PLOT_OUTPUT_PATH, dpi=300, bbox_inches="tight")
    print(f"Plot saved to {PLOT_OUTPUT_PATH}")

    # Display the plot
    plt.show(block=True)


if __name__ == "__main__":
    main()
