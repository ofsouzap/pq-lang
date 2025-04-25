from pathlib import Path
import matplotlib.pyplot as plt

from eval_lib.pq_intf import exec_pq_string
from eval_lib.investigation import (
    load_or_gen_results,
)

DATA_CACHE_PATH: Path = (
    Path(__file__).parent / Path("outputs") / Path("increasing_single_qt_fns.dat")
)
PLOT_OUTPUT_PATH: Path = (
    Path(__file__).parent / Path("outputs") / Path("eval-increasing_single_qt_fns.png")
)


def qt_defn() -> str:
    return f"""\
type list =
  | Nil of unit
  | Cons of int * list

qtype set =
  list
  |/ (x : int) -> (y : int) -> (zs : set)
    => Cons ((x : int), Cons ((y : int), (zs : set))) == (Cons (y, Cons (x, zs)))
  |/ (h : int) -> (ts : set)
    => Cons ((h : int), (ts : set)) == (Cons (h, Cons (h, ts)))
  |/ (h1 : int) -> (h2 : int) -> (ts : set)
    => Cons ((h1 : int), Cons ((h2 : int), (ts : set))) == (
      if h1 == h2
      then
        Cons (h1, ts)
      else
        Cons (h1, Cons (h2, ts))
      end
    )
"""


def fun_defn(i: int) -> str:
    return f"""\
let rec filter_{i} (arg : ((int -> bool) * set)) : set =
  match arg -> set with
  | ((p : int -> bool), (t : set)) ->
    match t -> set with
    | Nil (u : unit) -> Nil u
    | Cons ((xh : int), (xts : set)) ->
      if p xh
      then Cons (xh, filter_{i} (p, xts))
      else filter_{i} (p, xts)
      end
    end
  end
end
"""


def gen_source(n: int) -> str:
    outs: list[str] = []
    outs.append(qt_defn())

    for i in range(n):
        outs.append(fun_defn(i))

    return "\n".join(outs)


def main():
    n_values: list[int] = [5, 25, 45, 65, 85]

    all_results = load_or_gen_results(
        n_values,
        inp_gen=gen_source,
        exec=exec_pq_string,
        data_cache_path=DATA_CACHE_PATH,
        n_trials=5,
        n_repeats=10,
    )
    all_results_values = all_results.values.values()

    # Create the plot

    plot_vals_n = [res.seed for res in all_results_values]
    plot_vals_avg = [res.avg_time for res in all_results_values]
    plot_vals_std = [res.std for res in all_results_values]

    fig, ax = plt.subplots()

    # Plot with error bars

    ax.errorbar(
        plot_vals_n,
        plot_vals_avg,
        yerr=plot_vals_std,
        fmt="x-",
        color="tab:blue",
        ecolor="tab:gray",
        elinewidth=1,
        capsize=3,
    )

    ax.set_xlabel("Number of Quotient Type Functions")
    ax.set_ylabel("Time taken to check (s)")

    ax.grid()

    fig.suptitle(
        "Time taken to check for increasing number of functions on a single quotient types"
    )

    # Save the plot
    fig.savefig(PLOT_OUTPUT_PATH, dpi=300, bbox_inches="tight")
    print(f"Plot saved to {PLOT_OUTPUT_PATH}")

    # Display the plot
    plt.show(block=True)


if __name__ == "__main__":
    main()
