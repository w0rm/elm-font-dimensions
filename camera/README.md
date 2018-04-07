## opensolid/webgl

This is a small package containing conversion functions from the OpenSolid
point, direction and vector types (plus Elm's built-in `Color` type) to the
`Vec2`, `Vec3` and `Vec4` types used by the [`elm-community/webgl`](http://package.elm-lang.org/packages/elm-community/webgl/latest)
package. Functions are also provided for constructing WebGL `Mat4` model and
view matrices from OpenSolid `Frame3d` values.

All modules are intended to be imported using `as`, for example

```elm
import Color
import OpenSolid.Point3d as Point3d
import OpenSolid.WebGL.Color as Color
import OpenSolid.WebGL.Point3d as Point3d
```

This will allow you to call the conversion functions as if they were members of
the original modules:

```elm
Color.toVec4 Color.red
Point3d.toVec3 Point3d.origin
```
