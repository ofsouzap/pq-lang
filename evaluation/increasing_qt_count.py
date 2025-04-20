import time
import pickle
from pathlib import Path
from dataclasses import dataclass
from tqdm import tqdm
import statistics
import matplotlib.pyplot as plt

from eval_lib.pq_intf import exec_pq_string

DATA_CACHE_PATH: Path = (
    Path(__file__).parent / Path("outputs") / Path("increasing_qt_count.dat")
)
PLOT_OUTPUT_PATH: Path = (
    Path(__file__).parent / Path("outputs") / Path("increasing_qt_count.png")
)


def qt_defn(i: int) -> str:
    return f"""\
type list_{i} =
  | Nil{i} of unit
  | Cons{i} of int * list_{i}

qtype set_{i} =
  list_{i}
  |/ (x : int) -> (y : int) -> (zs : set_{i})
    => Cons{i} ((x : int), Cons{i} ((y : int), (zs : set_{i}))) == (Cons{i} (y, Cons{i} (x, zs)))
  |/ (h : int) -> (ts : set_{i})
    => Cons{i} ((h : int), (ts : set_{i})) == (Cons{i} (h, Cons{i} (h, ts)))
  |/ (h1 : int) -> (h2 : int) -> (ts : set_{i})
    => Cons{i} ((h1 : int), Cons{i} ((h2 : int), (ts : set_{i}))) == (
      if h1 == h2
      then
        Cons{i} (h1, ts)
      else
        Cons{i} (h1, Cons{i} (h2, ts))
      end
    )
"""


def fun_defn(i: int) -> str:
    return f"""\
let rec filter_{i} (arg : ((int -> bool) * set_{i})) : set_{i} =
  match arg -> set_{i} with
  | ((p : int -> bool), (t : set_{i})) ->
    match t -> set_{i} with
    | Nil{i} (u : unit) -> Nil{i} u
    | Cons{i} ((xh : int), (xts : set_{i})) ->
      if p xh
      then Cons{i} (xh, filter_{i} (p, xts))
      else filter_{i} (p, xts)
      end
    end
  end
end
"""


def gen_source(n: int) -> str:
    outs: list[str] = []

    for i in range(n):
        outs.append(qt_defn(i))
        outs.append(fun_defn(i))

    return "\n".join(outs)


@dataclass
class Results:
    results: list[float]
    n: int
    n_trials: int
    avg_time: float
    std: float


def time_for(
    n: int, *args, n_trials: int | None = None, n_repeats: int | None = None, **kwargs
) -> Results:
    n_trials_: int = n_trials or 3
    n_repeats_: int = n_repeats or 10

    results: list[float] = []

    for trial in tqdm(range(n_trials_), desc=f"Trials (n={n})", leave=False):
        source = gen_source(n)
        start = time.time()
        for _ in tqdm(
            range(n_repeats_),
            desc=f"Trial {trial+1}/{n_trials_}",
            leave=False,
        ):
            exec_pq_string(source, *args, **kwargs)
        end = time.time()

        time_elapsed = (end - start) / n_repeats_
        results.append(time_elapsed)

    return Results(
        results=results,
        n=n,
        n_trials=n_trials_,
        avg_time=sum(results) / len(results),
        std=statistics.stdev(results) if len(results) > 1 else 0.0,
    )


def time_for_all(n_values: list[int]) -> list[Results]:
    results: list[Results] = []
    for n in tqdm(n_values, desc="Testing different n values", leave=False):
        res = time_for(n, print_errors=True)
        results.append(res)
    return results


def load_or_gen_results() -> list[Results]:
    n_values: list[int] = [5, 25, 45, 65, 85]

    if DATA_CACHE_PATH.exists():
        with open(DATA_CACHE_PATH, "rb") as f:
            results = pickle.load(f)
        print(f"Loaded cached results from {DATA_CACHE_PATH}")
    else:
        results = time_for_all(n_values)
        print("Generated new results")

        with open(DATA_CACHE_PATH, "wb+") as f:
            pickle.dump(results, f)
        print(f"Saved results to {DATA_CACHE_PATH}")

    return results


def main():
    results = load_or_gen_results()

    # Create the plot
    plot_vals_n = [res.n for res in results]
    plot_vals_avg = [res.avg_time for res in results]
    plot_vals_std = [res.std for res in results]

    fig, ax = plt.subplots()

    # Plot with error bars
    ax.errorbar(plot_vals_n, plot_vals_avg, yerr=plot_vals_std, fmt="x-")

    ax.set_xlabel("Number of Quotient Types and Functions")
    ax.set_ylabel("Time taken to check (s)")

    # Save the plot
    fig.savefig(PLOT_OUTPUT_PATH, dpi=300, bbox_inches="tight")
    print(f"Plot saved to {PLOT_OUTPUT_PATH}")

    # Display the plot
    plt.show(block=True)


if __name__ == "__main__":
    main()
