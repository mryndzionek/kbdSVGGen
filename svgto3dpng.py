import bpy
import sys
import errno
import os.path

import mathutils

mm = 0.001

plates = {'bottomPlate': (1.5 * mm, 'steel'),
          'bottomNotch': (1.0 * mm, 'wood'),
          'spacerPlate': (8.0 * mm, 'acrylic'),
          'switchPlate': (1.5 * mm, 'steel'),
          'topPlate': (3 * mm, 'wood')}


def get_objects(l):
    return list(filter(lambda x: x.type in l, bpy.context.scene.objects))


def load_template():
    fn = 'template.blend'
    objects = []
    materials = []
    with bpy.data.libraries.load(fn) as (data_from, _):
        for name in data_from.objects:
            objects.append({'name': name})
        for name in data_from.materials:
            materials.append({'name': name})

    bpy.ops.wm.append(directory=fn+'/Object/', files=objects)
    bpy.ops.wm.append(directory=fn+'/Material/', files=materials)


def prepare():
    bpy.ops.object.select_all(action='DESELECT')

    objs = get_objects(['MESH', 'CURVE', 'LIGHT', 'CAMERA'])

    # delete all the existing meshes and curves
    for obj in objs:
        obj.select_set(True)
        bpy.ops.object.delete()

    # delete all the leftovers from previous SVG imports
    cs = filter(lambda x: x.name.startswith('atreus'), bpy.data.collections)
    for c in cs:
        bpy.data.collections.remove(c)


def create(fp, split):
    # import generated assembly file
    bpy.ops.import_curve.svg(filepath=fp)

    objs = get_objects(['CURVE'])
    gap = 0.05 * mm

    num_screws = 4 if split else 8

    key_locs = []
    screw_locs = []

    print(len(objs))
    
    for obj in objs[len(plates.keys()) + 1:-num_screws:2]:
        obj.select_set(True)
        bpy.context.scene.cursor.location = obj.location
        bpy.ops.object.origin_set(type='ORIGIN_GEOMETRY')
        key_locs.append((obj.location.x, obj.location.y))
        obj.select_set(False)

    for obj in objs[-num_screws:]:
        obj.select_set(True)
        bpy.context.scene.cursor.location = obj.location
        bpy.ops.object.origin_set(type='ORIGIN_GEOMETRY')
        screw_locs.append((obj.location.x, obj.location.y))
        obj.select_set(False)

    objs = get_objects(['MESH', 'CURVE'])

    for obj in objs[len(plates.keys()):]:
        obj.select_set(True)
        bpy.ops.object.delete()

    a = 0
    for i, (name, obj) in enumerate(zip(plates.keys(), objs)):
        bpy.context.view_layer.objects.active = None
        bpy.context.view_layer.objects.active = obj

        obj.name = name
        obj.select_set(True)

        (h, _) = plates[name]

        bpy.ops.object.mode_set(mode='EDIT')
        obj_data = obj.data
        obj_data.extrude = h / 2
        bpy.ops.object.mode_set(mode='OBJECT')

        bpy.ops.transform.translate(
            value=(0, 0, a + (h / 2)), constraint_axis=(True, True, True))
        if i != 1:
            a += h + gap

        obj.select_set(False)

    return key_locs, screw_locs


def move():
    # center the keyboard in x and y
    objs = get_objects(['CURVE'])

    for obj in objs:
        obj.select_set(True)

    objs = get_objects(['CURVE'])
    kbd = list(objs)[0]
    bpy.context.scene.cursor.location = kbd.location
    bpy.ops.object.origin_set(type='ORIGIN_GEOMETRY')
    offset = (kbd.location.x, kbd.location.y)
    bpy.ops.transform.translate(value=(-offset[0], -offset[1], 0))

    return offset


def adjust_materials():
    objs = get_objects(['CURVE'])
    for obj in objs:
        if obj.name in plates.keys():
            (_, mat) = plates[obj.name]
            obj.data.materials[0] = bpy.data.materials.get(mat)


def add_objects(name, kp, ofs, ang, split):
    keycap = bpy.data.objects[name]
    bpy.ops.object.select_all(action='DESELECT')

    for x, y in kp:
        a = -ang if split else -ang if x > offset[0] else ang

        keycap.select_set(True)
        bpy.ops.object.duplicate_move(OBJECT_OT_duplicate={"linked": False, "mode": 'TRANSLATION'},
                                      TRANSFORM_OT_translate={"value": (x - keycap.location.x - ofs[0],
                                                                        y - keycap.location.y - ofs[1], 0)})

        ov = bpy.context.copy()
        ov['area'] = [a for a in bpy.context.screen.areas if a.type == "VIEW_3D"][0]
        bpy.ops.transform.rotate(ov, value=a, orient_axis='Z')

        bpy.ops.object.light_add(type='AREA', radius=1, align='WORLD',
                                 location=(x - ofs[0], y - ofs[1], 0.004), scale=(1, 1, 1))
        light_ob = bpy.context.object
        light = light_ob.data
        light.energy = 0.025
        light.shape = 'DISK'
        light.size = 0.2
        light.color = (1, 0.507307, 0.01755)
        bpy.ops.object.select_all(action='DESELECT')


def render(rp):
    # render scene
    scene = bpy.context.scene
    scene.cycles.use_denoising = True
    scene.camera = bpy.data.objects['Camera']
    render = scene.render
    render.use_file_extension = True
    render.filepath = rp
    bpy.ops.render.render(write_still=True)
    bpy.ops.wm.save_as_mainfile(
        filepath='blender/' + os.path.basename(rp) + '.blend')


def export(rp):
    bpy.ops.object.select_all(action='DESELECT')
    bpy.data.objects['Plane'].select_set(True)
    bpy.ops.object.delete()
    # create STL file
    bpy.ops.export_mesh.stl(filepath=rp + '.stl')


argv = sys.argv
if "--" not in argv:
    argv = []
else:
    argv = argv[argv.index("--") + 1:]

assert(len(argv) == 3)

fp = argv[0]
angle = float(argv[1])
split = int(argv[2])

print(argv[2])

if not os.path.isfile(fp):
    raise OSError(
        errno.ENOENT, os.strerror(errno.ENOENT), fp
    )

rp = os.path.splitext(fp)[0]

prepare()
key_locs, screw_locs = create(fp, split)
offset = move()
load_template()
adjust_materials()
add_objects('Keycap', key_locs, offset, angle, split)
add_objects('Screw', screw_locs, offset, 0.3, split)
render(rp)
export(rp)
