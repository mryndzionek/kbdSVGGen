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

	heights = [1.5 * mm, 1.0 * mm, 8.0 * mm, 1.5 * mm, 3.0 * mm]
	gap = 0.05 * mm

	for obj in objs[len(heights):]:
		obj.select_set(True)
		bpy.ops.object.delete()

	a = 0
	for i, (h, obj) in enumerate(zip(heights, objs)):
		bpy.context.view_layer.objects.active = None
		bpy.context.view_layer.objects.active = obj
		
		obj.select_set(True)

		bpy.ops.object.mode_set(mode='EDIT')
		obj_data = obj.data
		obj_data.extrude = h / 2
		bpy.ops.object.mode_set(mode='OBJECT')

		bpy.ops.transform.translate(value = (0, 0, a + (h / 2)), constraint_axis = (True, True, True))
		if i != 1:
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


def change_to_glass(obj):
	TreeNodes = obj.active_material.node_tree
	links = TreeNodes.links

	matc = obj.active_material.node_tree.nodes["Principled BSDF"].inputs['Base Color'].default_value

	# Remove nodes (clean it)
	for node in TreeNodes.nodes:
		TreeNodes.nodes.remove(node)

	node_out = TreeNodes.nodes.new(type='ShaderNodeOutputMaterial')
	node_out.location = 200, 0

	# Glass BSDF
	node_glass = TreeNodes.nodes.new(type='ShaderNodeBsdfGlass')
	node_glass.distribution = 'BECKMANN'
	node_glass.location = 0, 180
	node_glass.distribution = 'GGX'
	# Preserve color
	node_glass.inputs['Color'].default_value = matc
	node_glass.inputs['Roughness'].default_value = 0.3
	node_glass.inputs['IOR'].default_value = 1.2

	links.new(node_glass.outputs[0], node_out.inputs[0])


def adjust_materials():
	objs = get_objects(['CURVE'])
	for obj in objs:
		if obj.active_material == None:
			obj.active_material = bpy.data.materials.new(name="SVGMat")
		elif obj.name == 'Curve.002':
			obj.active_material.blend_method = 'BLEND'
			obj.active_material.use_nodes = True
			change_to_glass(obj)
		else:
			obj.active_material.use_nodes = True
			tn = obj.active_material.node_tree.nodes["Principled BSDF"]
			tn.inputs['Roughness'].default_value = 0.2
			tn.inputs['Metallic'].default_value = 0.7

def adjust_view():
	# move camera closer
	camera = bpy.data.objects['Camera']
	camera.location = (0.3, -0.4, 0.3)
	looking_direction = camera.location - mathutils.Vector((0.0, 0.1, 0.0))
	rot_quat = looking_direction.to_track_quat('Z', 'Y')
	camera.rotation_euler = rot_quat.to_euler()
	camera.location = rot_quat @ mathutils.Vector((0.0, 0.0, 0.5))

def postprocess():
	# create background plane
	bpy.ops.mesh.primitive_plane_add(location=(0,0,0))
	o = bpy.context.selected_objects[0]
	o.active_material = bpy.data.materials.new(name="PlaneMat")
	o.active_material.use_nodes = True

	tn = o.active_material.node_tree.nodes["Principled BSDF"]
	tn.inputs['Base Color'].default_value = (0.0420277, 0.0420277, 0.0420277, 1)
	tn.inputs['Roughness'].default_value = 0.8
	tn.inputs['Specular'].default_value = 0.2
	tn.inputs['Metallic'].default_value = 0.6

	light_data = bpy.data.lights.new(name="light_", type='POINT')
	light_data.energy = 5
	light_data.use_shadow = True
	light_data.use_contact_shadow = True
	light_object = bpy.data.objects.new(name="light_", object_data=light_data)
	bpy.context.collection.objects.link(light_object)
	bpy.context.view_layer.objects.active = light_object
	light_object.location = (0, 0, 100 * mm)
		
def render(rp):
	bpy.context.scene.view_layers['View Layer'].cycles.use_denoising = True
	# render scene
	scene = bpy.context.scene
	render = scene.render
	render.use_file_extension = True
	render.filepath = rp
	bpy.ops.render.render(write_still=True)
	bpy.ops.wm.save_as_mainfile(filepath='blender/' + os.path.basename(rp) + '.blend')
	
def export(rp):
	# create STL file
	bpy.ops.export_mesh.stl(filepath=rp + '.stl')

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
