import numpy as np
import matplotlib.pyplot as plt
from scipy.integrate import odeint
from matplotlib import animation

plt.rcParams['keymap.save'] = ''
plt.rcParams['keymap.fullscreen'] = ''

class SpaceShip(object):
    def __init__(self):

        self.mass = 2
        self.Ibody = np.matrix([[7./4, 0, 0], [0, 3./4, 0], [0, 0, 2.]]) * self.mass
        self.IbodyInv = np.linalg.inv(self.Ibody)

        self.x = np.matrix([[0], [0], [0]])
        self.R = np.matrix([[1, 0, 0], [0, 1, 0], [0, 0, 1]])
        self.P = np.matrix([[0], [0], [0]])
        self.L = np.matrix([[0], [0], [0]])

        self.Iinv = np.matrix([[1, 0, 0], [0, 1, 0], [0, 0, 1]])
        self.v = np.matrix([[0], [0], [0]])
        self.omega = np.matrix([[0], [0], [0]])

        self.Force = np.matrix([[0], [0], [0]])
        self.Torque = np.matrix([[0], [0], [0]])

        self.model = [[-1., -1., 0],
                      [ 1., -1., 0],
                      [ 0.,  3., 0],
                      [-1., -1., 0]]


        self.engines = []

        #     self.engines = [[0., (  0, -1, 0), (0, 1, 0)],
        #                     [0., ( 0.33,  2, 0), (-0.7071, -0.7071, 0)],
        #                     [0., (-0.33,  2, 0), ( 0.7071, -0.7071, 0)]]
        # elif self.type == 'three':
        #     self.engines = [[0., (  0, -1, 0), (0, 1, 0)],
        #                     [0., ( 0.33,  2, 0), (-0.7071, -0.7071, 0)],
        #                     [0., (-0.33,  2, 0), ( 0.7071, -0.7071, 0)]]
        # elif self.type == 'three':
        #     self.engines = [[0., (  0.5, -1, 0), (0, 1, 0)],
        #                     [0., ( -0.5, -1, 0), (0, 1, 0)],
        #                     [0., ( 0.33,  2, 0), (-0.7071, -0.7071, 0)],
        #                     [0., (-0.33,  2, 0), ( 0.7071, -0.7071, 0)]]

    def to_center(self):
        self.x = np.matrix([[0], [0], [0]])
        # self.R = np.matrix([[1, 0, 0], [0, 1, 0], [0, 0, 1]])

    def stop(self):
        self.P = np.matrix([[0], [0], [0]])
        self.L = np.matrix([[0], [0], [0]])

    def to_state(self):
        return self.x.reshape((1,3)).tolist()[0] + \
            self.R.reshape((1,9)).tolist()[0] + \
            self.P.reshape((1,3)).tolist()[0] + \
            self.L.reshape((1,3)).tolist()[0]

    def from_state(self, state):
        self.x = np.matrix(state[0:0+3]).reshape((3,1))
        self.R = np.matrix(state[3:3+9]).reshape((3,3))
        self.P = np.matrix(state[12:12+3]).reshape((3,1))
        self.L = np.matrix(state[15:15+3]).reshape((3,1))
        self.v = self.P / self.mass
        self.Iinv = self.R * self.IbodyInv * self.R.T
        self.omega = self.Iinv * self.L

    def compute_forces_and_torques(self):
        total_force = np.matrix([[0], [0], [0]])
        total_torque = np.matrix([0, 0, 0])
        for f, r, d in self.engines:
            total_force = total_force + f * np.matrix(d).T
            total_torque = total_torque + np.cross(np.matrix(r), f * np.matrix(d))
        self.Force = self.R * total_force
        self.Torque = self.R * total_torque.T

    def star(self, a):
        return np.matrix([[0, -a[2], a[1]], [a[2], 0, -a[0]], [-a[1], a[0], 0]])

    def ddt(self):
        self.compute_forces_and_torques()
        Rdot = self.star(self.omega) * self.R
        return self.v.reshape((1,3)).tolist()[0] + \
            Rdot.reshape((1,9)).tolist()[0] + \
            self.Force.reshape((1,3)).tolist()[0] + \
            self.Torque.reshape((1,3)).tolist()[0]

    def model_in_world(self):
        r = []
        for p in self.model:
            q = self.x + self.R * np.matrix(p).T
            r.append((q.item(0), q.item(1)))
        return r

    def engines_in_world(self):
        r = []
        for f, p, d in self.engines:
            q = self.x + self.R * np.matrix(p).T
            r.append( ((q.item(0), q.item(1)), f / 2.) )
        return r

    def onkey(self, event):
        if event.key == ' ':
            self.stop()
        elif event.key == 'z':
            self.to_center()
        else:
            pass

class CubeShip(SpaceShip):
    def __init__(self):
        super(CubeShip, self).__init__()
        self.engines = [[0., (  0.0, -1, 0), (0, 1, 0)],
                        [0., (   -1,  0, 0), (0, 1, 0)],
                        [0., (    1,  0, 0), (0, 1, 0)]]

    def onkey(self, event):
        df = 0.05
        incr = {'q':1, 'w':0, 'e':2, 'r':0}
        decr = {'a':1, 's':0, 'd':2, 'f':0}
        maxf = {0:1, 1:1, 2:1, 3:0.4}
        if event.key in incr:
            e = incr[event.key]
            self.engines[e][0] = min(self.engines[e][0] + df, maxf[e])
        elif event.key in decr:
            e = decr[event.key]
            self.engines[e][0] = max(self.engines[e][0] - df, 0.0)
        else:
            super(CubeShip, self).onkey(event)

class TrigShip(SpaceShip):
    def __init__(self):
        super(TrigShip, self).__init__()
        self.engines = [[0., (  0.0, -1, 0), (0, 1, 0)],
                        [0., (   -1,  -1, 0), (0, 1, 0)],
                        [0., (    1,  -1, 0), (0, 1, 0)]]

    def onkey(self, event):
        df = 0.05
        incr = {'q':1, 'w':0, 'e':2, 'r':0}
        decr = {'a':1, 's':0, 'd':2, 'f':0}
        maxf = {0:1, 1:1, 2:1, 3:0.4}
        if event.key in incr:
            e = incr[event.key]
            self.engines[e][0] = min(self.engines[e][0] + df, maxf[e])
        elif event.key in decr:
            e = decr[event.key]
            self.engines[e][0] = max(self.engines[e][0] - df, 0.0)
        else:
            super(TrigShip, self).onkey(event)


ship = CubeShip()
ship = TrigShip()

def func(y, t, ship):
    ship.from_state(y)
    return ship.ddt()

## drawing and keys handling
fig = plt.figure(figsize=(20, 12))
#ax = plt.axes(xlim=(-20, 20), ylim=(-20, 20), aspect='equal')
ax =fig.add_axes([0.25, 0.05, 0.9, 0.9], xlim=(-20, 20), ylim=(-20, 20), aspect='equal')

poly = plt.Polygon([[0,0],], facecolor='red', edgecolor='none')
engines = [plt.Circle((0, 0), 0, fc='b', edgecolor='none'),
           plt.Circle((0, 0), 0, fc='b', edgecolor='none'),
           plt.Circle((0, 0), 0, fc='b', edgecolor='none'),
           plt.Circle((0, 0), 0, fc='b', edgecolor='none')]
traj = plt.Line2D((0,), (0,), lw=2., ls='--')

def onclick(event):
    print 'button=%d, x=%d, y=%d, xdata=%f, ydata=%f'%(
        event.button, event.x, event.y, event.xdata, event.ydata)

def onkey(event):
    # print('you pressed', event.key, event.xdata, event.ydata)
    if event.key == 'k':
        global goal
        goal = np.matrix([[0], [0], [0]])
    ship.onkey(event)

def init():
    for e in engines:
        ax.add_patch(e)
    ax.add_patch(poly)
    ax.add_line(traj)
    return poly,


theta0 = None
thetai = 0.0
goal = np.matrix([[100], [100], [0]])

def animate(i):
    ## initial point
    y0 = ship.to_state()

    ## PID controller
    global theta0, thetai, goal

    x = np.matrix(y0[0:0+3]).reshape((3,1))
    R = np.matrix(y0[3:3+9]).reshape((3,3))

    direction = goal - x; direction = direction / np.linalg.norm(direction)
    orientation = R * np.matrix([[0], [1], [0]])
    theta = np.arctan2(np.cross(direction.T, orientation.T) * np.matrix([[0], [0], [1]]), direction.T * orientation)
    #alpha = np.arccos(y0[0][3])
    dt = 0.02
    Kp, Ki, Kd = 1, 0.1, 1.
    dtheta = (theta - theta0) / dt if theta0 != None else 0

    u = Kp * theta + Ki * thetai + Kd * dtheta

    max_thrust = 0.5
    thrust = max(-max_thrust, min(max_thrust, u.item(0)))
    print(thrust)

    if thrust > 1e-2:
        ship.engines[1][0] = +thrust
        ship.engines[2][0] = 0
        ship.engines[0][0] = 0.9
    elif thrust < -1e-2:
        ship.engines[1][0] = 0
        ship.engines[2][0] = -thrust
        ship.engines[0][0] = 0.9
    else:
        ship.engines[1][0] = max_thrust
        ship.engines[2][0] = max_thrust
        ship.engines[0][0] = 1.
        print '--------------- full', dtheta, thrust

    theta0 = theta
    thetai += theta * dt

    ## compute trajectory
    #y = odeint(func, y0, np.arange(0, 3, 0.1), (ship,), atol=1e-1, rtol=1e-1)
    #traj.set_xdata(y.T[0])
    #traj.set_ydata(y.T[1])
    # compute next step
    y = odeint(func, y0, [0, 0.02], (ship,))

    ## model -> world
    for e, pos in zip(engines, ship.engines_in_world()):
        e.center = pos[0]
        e.radius = pos[1]
    poly.xy = ship.model_in_world()
    result = list(engines)
    result.append(poly)
    result.append(traj)
    return result

fig.canvas.mpl_connect('button_press_event', onclick)
fig.canvas.mpl_connect('key_press_event', onkey)
anim = animation.FuncAnimation(fig, animate,
                               init_func=init,
                               frames=None,
                               interval=20,
                               blit=True)
plt.show()
