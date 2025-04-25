from typing import List, Dict, Callable, TypeVar, Generic, Optional
import time
import pickle
from pathlib import Path
from dataclasses import dataclass
from tqdm import tqdm
import statistics


TestGenSeed = TypeVar("TestGenSeed")
TestInput = TypeVar("TestInput")


@dataclass
class SingleResult(Generic[TestGenSeed]):
    results: List[float]
    seed: TestGenSeed
    n_trials: int
    avg_time: float
    std: float


@dataclass
class AllResults(Generic[TestGenSeed]):
    values: Dict[TestGenSeed, SingleResult[TestGenSeed]]


def time_for(
    seed: TestGenSeed,
    inp_gen: Callable[[TestGenSeed], TestInput],
    exec: Callable[[TestInput], None],
    *args,
    n_trials: Optional[int] = None,
    n_repeats: Optional[int] = None,
    **kwargs,
) -> SingleResult:
    n_trials_: int = n_trials or 5
    n_repeats_: int = n_repeats or 10

    results: List[float] = []

    for trial in tqdm(range(n_trials_), desc=f"Trials", leave=False):
        inp = inp_gen(seed)
        start = time.time()
        for _ in tqdm(
            range(n_repeats_),
            desc=f"Trial {trial+1}/{n_trials_}",
            leave=False,
        ):
            exec(inp, *args, **kwargs)
        end = time.time()

        time_elapsed = (end - start) / n_repeats_
        results.append(time_elapsed)

    return SingleResult[TestGenSeed](
        results=results,
        seed=seed,
        n_trials=n_trials_,
        avg_time=sum(results) / len(results),
        std=statistics.stdev(results) if len(results) > 1 else 0.0,
    )


def load_or_gen_results(
    seeds: List[TestGenSeed], inp_gen, exec, *args, data_cache_path: Path, **kwargs
) -> AllResults:

    all_results: AllResults[TestGenSeed]

    if data_cache_path.exists():
        with open(data_cache_path, "rb") as f:
            all_results = pickle.load(f)
        print(f"Loaded cached results from {data_cache_path}")
    else:
        all_results = AllResults(values={})
        print(f"Cache not found, will generate new results")

    existing_seeds = set(seed for seed in all_results.values.keys())
    remaining_seeds = [seed for seed in seeds if seed not in existing_seeds]

    if remaining_seeds:
        print(f"Remaining seeds values to test: {remaining_seeds}")

        for seed in tqdm(
            remaining_seeds, desc="Testing different seeds values", leave=False
        ):
            res = time_for(seed, inp_gen, exec, print_errors=True, *args, **kwargs)
            all_results.values[res.seed] = res

            with open(data_cache_path, "wb+") as f:
                pickle.dump(all_results, f)

        print("Generated and saved new results")

    return all_results
