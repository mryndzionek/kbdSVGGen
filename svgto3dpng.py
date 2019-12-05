import bpy
import sys
import errno
import os.path

import mathutils

argv = sys.argv
if "--" not in argv:
    argv = []
else:
    argv = argv[argv.index("--") + 1:]

fp = argv[0]

if not os.path.isfile(fp):
    raise OSError(
        errno.ENOENT, os.strerror(errno.ENOENT), fp
    )

bpy.ops.object.select_all(action='DESELECT')

objs = filter(lambda x: x.type in ['MESH','CURVE'], bpy.context.scene.objects)

# delete all the existing meshes and curves
for obj in objs:
    obj.select_set(True)
    bpy.ops.object.delete()

# delete all the leftovers from previous SVG imports
cs = filter(lambda x: x.name.startswith('atreus'), bpy.data.collections)
for c in cs:
    bpy.data.collections.remove(c)

# import generated assembly file
bpy.ops.import_curve.svg(filepath = fp)

objs = list(filter(lambda x: x.type in ['MESH','CURVE'], bpy.context.scene.objects))

mm = 0.001
heights = [3.0 * mm, 6.0 * mm, 1.5 * mm, 3.0 * mm]

for obj in objs[len(heights):]:
    obj.select_set(True)
    bpy.ops.object.delete()

a = 0
for h, obj in zip(heights, objs):
    bpy.context.view_layer.objects.active = None
    bpy.context.view_layer.objects.active = obj
    
    obj.select_set(True)

    bpy.ops.object.convert(target='MESH')
    bpy.ops.object.mode_set(mode='EDIT')
    bpy.ops.mesh.select_mode(type='FACE')
    bpy.ops.mesh.select_all(action='SELECT')

    bpy.ops.mesh.extrude_region_move(
        TRANSFORM_OT_translate={"value":(0, 0, h)}
    )

    bpy.ops.object.mode_set(mode='OBJECT')
    
    bpy.ops.transform.translate(value = (0, 0, a), constraint_axis = (True, True, True))
    a += h
    
    obj.select_set(False)
    
objs = filter(lambda x: x.type in ['MESH'], bpy.context.scene.objects)
for obj in objs:
    obj.select_set(True)
    
bpy.ops.object.join()

# center the keyboard in x and y
objs = filter(lambda x: x.type in ['MESH'], bpy.context.scene.objects)
obj = list(objs)[0]
bpy.context.scene.cursor.location = obj.location
bpy.ops.object.origin_set(type='ORIGIN_GEOMETRY')
bpy.ops.transform.translate(value=(-obj.location.x, -obj.location.y, 0))

# move camera closer
camera = bpy.data.objects['Camera']
looking_direction = camera.location - mathutils.Vector((0.0, 0.0, 0.0))
rot_quat = looking_direction.to_track_quat('Z', 'Y')
camera.rotation_euler = rot_quat.to_euler()
camera.location = rot_quat @ mathutils.Vector((0.0, 0.0, 0.5))

# render scene
scene = bpy.context.scene
render_path = os.path.splitext(fp)[0]
render = scene.render
render.use_file_extension = True
render.filepath = render_path
bpy.ops.render.render(write_still=True)

# create STL file
bpy.ops.export_mesh.stl(filepath=render_path + '.stl')

bpy.ops.wm.save_as_mainfile(filepath='blender/' + os.path.basename(render_path) + '.blend')
