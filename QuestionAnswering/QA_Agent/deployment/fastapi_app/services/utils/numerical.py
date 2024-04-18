import numpy as np
import numpy.typing as npt


def cosine_similarity(a: npt.ArrayLike, b: npt.ArrayLike):
    if not isinstance(a, np.ndarray):
        a = np.array(a)
    if not isinstance(b, np.ndarray):
        b = np.array(b)

    if a.ndim == 1:
        if b.ndim != 1:
            raise ValueError(
                "Input arrays must have the same dimensions. Found {a} and {b}.".format(
                    a=str(a.shape), b=str(b.shape)
                )
            )
        a = a / np.linalg.norm(a)
        b = b / np.linalg.norm(b)
    else:
        a = a / np.linalg.norm(a, axis=1)[:, None]
        b = b / np.linalg.norm(b, axis=1)[:, None]
    return a @ b.T
