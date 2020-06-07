# Rebel Game Engine
A 3D game engine using C with Lisp scripting.

The engine is an application of the lessons I've learned researching game engines and poking around various open source projects. I don't plan to use it for production but it might eventually evolve into something that will. The primary goal for now is implementing the barest essentials.

For more information check out [the wiki](https://github.com/accidentalrebel/rebel-game-engine/wiki/Getting-Started) for more info.

### Engine is still under heavy development

![screenshot-1](https://raw.githubusercontent.com/accidentalrebel/rebel-game-engine/master/images/rebel-screenshot-1.png)

## Sample Code
Simple program that displays a sprite with keyboard movement.

```scheme
   (define *box-sprite*)
   (define *box-pos*)

   (define (init)
     (set! *box-sprite* (sprite:create "assets/textures" "tile.png"))
     (set! *box-pos* (vec3:create 400 300 0)))

   (define (update)
     (window:clear)

     (when (key:down? KEY_A)
       (vec3_x! *box-pos* (+ (vec3_x *box-pos*) 1)))
     (when (key:down? KEY_D)
       (vec3_x! *box-pos* (- (vec3_x *box-pos*) 1)))

     (let ((tint (vec3:create 1 0 1)))
       (sprite:draw *box-sprite* *box-pos* 50 50 tint #f))

     (window:swap))
```

## Roadmap
v1.0
- [x] Window management
- [x] Drawing sprites
- [x] Coordinate system
- [x] Scripting Integration
- [x] Multiplatform development
- [x] Camera System
- [x] Basic Lighting
- [ ] Sprite transparency
- [ ] Cubemaps
- [ ] Loading 3D models
- [ ] Scenes
- [ ] Text
- [ ] Audio
- [ ] Save system
- [ ] Scheme examples
- [ ] Deployment

## Getting started
Details on how to download the engine, build, and run a sample program can be found on the wiki [here](https://github.com/accidentalrebel/rebel-game-engine/wiki/Getting-Started).

## Dependencies / External Libraries
As much as I want to implement everything myself, these libraries are just better so no need to re-invent the wheel.

  * [Glad](https://github.com/Dav1dde/glad) - OpenGL Loader-Generator
  * [GLFW](https://www.glfw.org/) - For OpenGL and window handling
  * [CGLM](https://github.com/recp/cglm) - Highly optimized graphics math for C
  * [stb_image](https://github.com/nothings/stb) - Image loader
  * [shader.h](https://learnopengl.com/code_viewer_gh.php?code=includes/learnopengl/shader_s.h) - LearnOpenGL's shader class
  * [chicken-scheme](https://www.call-cc.org/) - Enables lisp scripting

## Many thanks
This project wouldn't be possible without these:

  * [Handmade Hero](https://handmadehero.org/) - The project that started my journey to appreciating hand-made programs and applications.
  * [LazyFoo's SDL Tutorials](http://lazyfoo.net/tutorials/SDL/index.php) - I did not use SDL for this engine but I learned about it using these excellent tutorials.
  * [LearnOpenGL](https://learnopengl.com/) - An excellent site containing tutorials about OpenGL. Ramps up from beginner to advanced concepts quite nicely.
  * [Raylib](https://www.raylib.com/) - An excellent game programming library that has very simple and readable code.
  * [Raylib-Chibi](https://github.com/VincentToups/raylib-chibi) - A chibi-scheme implementation for the Raylib library. Scripting on my engine won't be possible without this as a reference.
  * [Myriad Game Engine](https://github.com/jobtalle/Myriad) - A bare bones game engine that I used to learn about OpenGL loading without GLFW.

