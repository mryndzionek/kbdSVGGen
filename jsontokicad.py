import os
import sys
import json
import math

import pcbnew

SCALE = 1000000
SIZES = {'A4': (297, 210), 'A3': (420, 297), 'A2': (594, 420)}


class Ctx:
    def __init__(self, cfg, name):
        self.name = name
        self.angle = cfg['angle']
        self.isSplit = cfg['split']
        width = cfg['width']
        height = cfg['height']
        self.size = 'A4'
        for s, (w, _) in SIZES.items():
            if width < w:
                self.size = s
                break
        self.board = pcbnew.LoadBoard('pcb/template_' + self.size + '.kicad_pcb')
        self.board.GetTitleBlock().SetTitle(self.name)
        self.rows = cfg['rows']
        self.columns = cfg['columns']
        self.angle = cfg['angle']
        self.switches = [(s[0], s[1]) for s in cfg['switches']]
        self.ts = self.switches[self.columns * self.rows:]
        self.tn = len(self.ts)
        self.tsn = math.ceil(len(self.ts) / 2)
        if not self.isSplit:
            self.tsn = 2 * self.tsn
        w, h = SIZES[self.size]
        self.center = (w / 2, (h / 2) + (height / 2))

        self.colNets = [pcbnew.NETINFO_ITEM(self.board, "COL_{}".format(n))
                        for n in range(1,  (self.columns if self.isSplit else 2 * self.columns) + self.tsn + 1)]
        self.rowNets = [pcbnew.NETINFO_ITEM(self.board, "ROW_{}".format(n))
                        for n in range(1, self.rows + 1)]
        for net in self.colNets + self.rowNets:
            self.board.Add(net)

        self.io = pcbnew.PCB_IO()

    def save(self):
        self.board.Save('gen/' + self.name + '.kicad_pcb')


def rotate(origin, point, deg):
    ox, oy = origin
    px, py = point

    angle = math.radians(deg)
    qx = ox + math.cos(angle) * (px - ox) - math.sin(angle) * (py - oy)
    qy = oy + math.sin(angle) * (px - ox) + math.cos(angle) * (py - oy)
    return qx, qy


def getPads(module):
    return {pad.GetPadName(): pad for pad in module.Pads()}


def addDiode(ctx, x, y, c, r, angle):
    diode = ctx.io.FootprintLoad(
        'pcb/', 'D_DO-35_SOD27_P3.81mm_Vertical_AnodeUp')
    diode.SetReference("D%d-%d" % (c, r))

    xn, yn = rotate((x, y), (x - 12, y + 12), -angle)
    diode.SetPosition(pcbnew.wxPoint(
        (ctx.center[0] + xn) * SCALE, (ctx.center[1] + yn) * SCALE))
    diode.SetOrientation((90 + angle) * 10)

    ctx.board.Add(diode)
    return diode


def addSwitch(ctx, x, y, c, r, angle):
    switch = ctx.io.FootprintLoad('pcb/', 'SW_Cherry_MX_1.00u_PCB')
    switch.SetReference("S%d-%d" % (c, r))
    switch.SetPosition(pcbnew.wxPoint(
        (ctx.center[0] + x) * SCALE, (ctx.center[1] - y) * SCALE))
    switch.SetOrientation(angle * 10)
    ctx.board.Add(switch)

    diode = addDiode(ctx, x, -y, c, r, angle)

    sPads = getPads(switch)
    dPads = getPads(diode)
    
    dPads['1'].SetNetCode(ctx.rowNets[r - 1].GetNet())
    sPads['1'].SetNetCode(ctx.colNets[c - 1].GetNet())

    net = pcbnew.NETINFO_ITEM(ctx.board, "SD_{}_{}".format(c, r))
    ctx.board.Add(net)

    sPads['2'].SetNetCode(net.GetNet())
    dPads['2'].SetNetCode(net.GetNet())


def rotated(x, y, a):
    return rotate((x, y), (x + (2 * 1.27), y + (4 * 1.27)), a)


def addSwitches(ctx):
    for r in range(0, ctx.rows):
        for c in range(0, ctx.columns):
            i = c * ctx.rows + r
            x, y = ctx.switches[i]
            xn, yn = rotated(x, y, ctx.angle)
            addSwitch(ctx, xn, yn, (0 if ctx.isSplit else ctx.columns) + ctx.tsn +
                      c + 1, ctx.rows - r, ctx.angle)
            if not ctx.isSplit:
                xn, yn = rotated(-x, y, -ctx.angle)
                addSwitch(ctx, xn, yn, ctx.columns -
                          c, ctx.rows - r, -ctx.angle)

    for i, (x, y) in enumerate(ctx.ts):
        r = ctx.rows - (2 - (i % 2)) + 1
        c1 = ctx.columns + math.floor(i / 2) + 1
        c2 = (1 if ctx.isSplit else ctx.columns + ctx.tsn) - math.floor(i / 2)
        xn, yn = rotated(x, y, ctx.angle)
        addSwitch(ctx, xn, yn, c2, r, ctx.angle)
        if not ctx.isSplit:
            xn, yn = rotated(-x, y, -ctx.angle)
            addSwitch(ctx, xn, yn, c1, r, -ctx.angle)


cfg_filename = sys.argv[1]
name = os.path.splitext(os.path.basename(cfg_filename))[0]

with open(cfg_filename) as f:
    cfg = json.load(f)
    ctx = Ctx(cfg, name)

    addSwitches(ctx)
    ctx.save()
