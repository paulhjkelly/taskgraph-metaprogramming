include $(top_srcdir)/binaries_common.mk

RAYTRACER_SOURCES = Finish.cc Intersection.cc Light.cc World.cc Vector3D.cc raytracer.cc Camera.h Colour.h Finish.h Intersection.h Light.h Math3D.h Object.h Ray.h Sphere.h Vector3D.h VectorOp.h World.h

bin_PROGRAMS = raytracer-tg raytracer-cpp
raytracer_tg_SOURCES = $(RAYTRACER_SOURCES)
raytracer_cpp_SOURCES = $(RAYTRACER_SOURCES)
raytracer_tg_CXXFLAGS = -DSPECIALISE
