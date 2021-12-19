import numpy as np


cos = [1, 0, -1, 0]
sin = [0, 1, 0, -1]


x_rots = []
for c, s in zip(cos, sin):
    rot = np.array([[1, 0, 0], [0, c, -s], [0, s, c]])
    x_rots.append(rot)

y_rots = []
for c, s in zip(cos, sin):
    rot = np.array([[c, 0, -s], [0, 1, 0], [s, 0, c]])
    y_rots.append(rot)


z_rots = []
for c, s in zip(cos, sin):
    rot = np.array([[c, -s, 0], [s, c, 0], [0, 0, 1]])
    z_rots.append(rot)


all_rots = []
for x_rot in x_rots:
    for y_rot in y_rots:
        for z_rot in z_rots:
            all_rots.append(x_rot @ y_rot @ z_rot)


all_rots = set([tuple(rot.reshape(-1).tolist()) for rot in all_rots])
for rot in all_rots:
    print("[|{}|];".format(";".join([str(val) for val in rot])))