import json
import logging
from collections.abc import Callable, Sequence
from dataclasses import asdict, is_dataclass

from optuna import Trial


def suggest_int(low: int, high: int, log: bool = False) -> Callable[[str, Trial], int]:
    return lambda name, trial: trial.suggest_int(name=name, low=low, high=high, log=log)


def suggest_from[T](choices: Sequence[T]) -> Callable[[str, Trial], T]:
    return lambda name, trial: trial.suggest_categorical(name=name, choices=choices)  # type: ignore


suggest_bool: Callable[[str, Trial], bool] = suggest_from([False, True])


def get_logger(name: str) -> logging.Logger:
    logger = logging.getLogger(name)
    logger.setLevel(logging.DEBUG)
    handler = logging.StreamHandler()
    handler.setLevel(logging.DEBUG)
    handler.setFormatter(logging.Formatter("%(asctime)s %(name)s %(levelname)s %(message)s"))
    logger.addHandler(handler)
    return logger


def pretty_print_json(obj: object) -> str:
    if is_dataclass(obj):
        obj = asdict(obj)
    return json.dumps(obj, indent=2, sort_keys=True)
