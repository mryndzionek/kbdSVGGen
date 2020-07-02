import bpy
import sys
import errno
import os.path

import mathutils

mm = 0.001

def get_objects(l):
    return list(filter(lambda x: x.type in l, bpy.context.scene.objects))

def prepare():
    bpy.ops.object.select_all(action='DESELECT')

    objs = get_objects(['MESH','CURVE'])

    # delete all the existing meshes and curves
    for obj in objs:
        obj.select_set(True)
        bpy.ops.object.delete()

    # delete all the leftovers from previous SVG imports
    cs = filter(lambda x: x.name.startswith('atreus'), bpy.data.collections)
    for c in cs:
        bpy.data.collections.remove(c)
        
def create(fp):
    # import generated assembly file
    bpy.ops.import_curve.svg(filepath = fp)

    objs = get_objects(['MESH','CURVE'])

    heights = [3.0 * mm, 3.0 * mm, 3.0 * mm, 1.5 * mm, 3.0 * mm]
    gap = 0.05 * mm

    for obj in objs[len(heights):]:
        obj.select_set(True)
        bpy.ops.object.delete()

    a = 0
    for h, obj in zip(heights, objs):
        bpy.context.view_layer.objects.active = None
        bpy.context.view_layer.objects.active = obj
        
        obj.select_set(True)

        bpy.ops.object.mode_set(mode='EDIT')
        obj_data = obj.data
        obj_data.extrude = h / 2
        bpy.ops.object.mode_set(mode='OBJECT')

        bpy.ops.transform.translate(value = (0, 0, a + (h / 2)), constraint_axis = (True, True, True))
        a += h + gap
        
        obj.select_set(False)
        
def join():
    objs = get_objects(['CURVE'])
    for obj in objs:
        obj.select_set(True)
    bpy.ops.object.join()
    
def move():
    # center the keyboard in x and y
    objs = get_objects(['CURVE'])

    for obj in objs:
        obj.select_set(True)

    objs = get_objects(['CURVE'])
    kbd = list(objs)[0]
    bpy.context.scene.cursor.location = kbd.location
    bpy.ops.object.origin_set(type='ORIGIN_GEOMETRY')
    bpy.ops.transform.translate(value=(-kbd.location.x, -kbd.location.y, 0))

def adjust_materials():
	objs = get_objects(['CURVE'])
	for i, obj in enumerate(objs):
		if obj.active_material == None:
			obj.active_material = bpy.data.materials.new(name="SVGMat" + str(i))
		elif i in [1, 2]:
			c = obj.active_material.diffuse_color
			obj.active_material = bpy.data.materials.new(name="SVGMat" + str(i))
			obj.active_material.diffuse_color = c
			obj.active_material.blend_method = 'BLEND'
			obj.active_material.use_nodes = True
			obj.active_material.node_tree.nodes["Principled BSDF"].inputs[18].default_value = 0.5
		else:
			c = obj.active_material.diffuse_color
			obj.active_material = bpy.data.materials.new(name="SVGMat" + str(i))
			obj.active_material.diffuse_color = c

    
def adjust_view():
    # move camera closer
    camera = bpy.data.objects['Camera']
    camera.location = (0.3, -0.4, 0.3)
    looking_direction = camera.location - mathutils.Vector((0.0, 0.1, 0.0))
    rot_quat = looking_direction.to_track_quat('Z', 'Y')
    camera.rotation_euler = rot_quat.to_euler()
    camera.location = rot_quat @ mathutils.Vector((0.0, 0.0, 0.5))

def postprocess():
    # create backgroung plane
    mat = bpy.data.materials.new("BKG")
    mat.diffuse_color = (0.0, 0.0, 0.0, 1.0)
    mat.roughness = 0.7
    bpy.ops.mesh.primitive_plane_add(location=(0,0,0))
    o = bpy.context.selected_objects[0] 
    o.active_material = mat

    light_data = bpy.data.lights.new(name="light_", type='POINT')
    light_data.energy = 5
    light_data.use_shadow = True
    light_data.use_contact_shadow = True
    light_object = bpy.data.objects.new(name="light_", object_data=light_data)
    bpy.context.collection.objects.link(light_object)
    bpy.context.view_layer.objects.active = light_object
    light_object.location = (0, 0, 100 * mm)

    bpy.context.scene.world.light_settings.use_ambient_occlusion = True
    bpy.context.scene.world.light_settings.ao_factor = 0.5
    
def render(rp):
    # render scene
    scene = bpy.context.scene
    render = scene.render
    render.use_file_extension = True
    render.filepath = rp
    bpy.ops.render.render(write_still=True)
    
def export(rp):
    # create STL file
    bpy.ops.export_mesh.stl(filepath=rp + '.stl')
    bpy.ops.wm.save_as_mainfile(filepath='blender/' + os.path.basename(rp) + '.blend')

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

rp = os.path.splitext(fp)[0]
    
prepare()
create(fp)
move()
adjust_materials()
adjust_view()
export(rp)
postprocess()
render(rp)



