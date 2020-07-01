# Rebel Game Engine
A 3D game engine using C with Lisp scripting.

The engine is an application of the lessons I've learned researching game engines and poking around various open source projects. I don't plan to use it for production but it might eventually evolve into something that will. The primary goal for now is implementing the barest essentials.

For more information check out [the wiki](https://github.com/accidentalrebel/rebel-game-engine/wiki/Getting-Started) for more info.

### Engine is still under heavy development

![screenshot-1](https://raw.githubusercontent.com/accidentalrebel/rebel-game-engine/master/images/rebel-screenshot-1.png)

## Sample Code
Simple program that loads a 3d model and draws it. Also has first person camera movement.

```scheme
(define *basic-shader*)
(define *backpack*)

;; Setup the first person camera extension
(include-relative "../../extensions/fpcam")

(define (init)
  ;; Load the model object
  ;; model:load also loads the textures automatically
  (set! *backpack* 
	(model:load "assets/models/backpack/backpack.obj"))

  ;; Load a basic shader
  (set! *basic-shader* 
	(shader:create "shaders/model-loading.vs" "shaders/model-loading.fs")))

(define (update)
  ;; This is needed for the first person camera extension to work
  (fpcam:update))

(define (render)
  (window:clear '(0.1 0.1 0.1))

  ;; Use the basic-shader
  (shader:use *basic-shader*)

  ;; Draw the backpack model
  (model:draw *backpack* '(0 0 0) '(1 1 1))
  
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
- [x] Model loading
- [ ] Sprite transparency
- [ ] Cubemaps
- [ ] Scenes
- [ ] Text
- [ ] Audio
- [ ] Save system
- [ ] Scheme examples
- [ ] Deployment

## Getting started
Details on how to download the engine, build, and run a sample program can be found on the wiki [here](https://github.com/accidentalrebel/rebel-game-engine/wiki/Getting-Started).

You could also check out the examples folder.

## Dependencies / External Libraries
As much as I want to implement everything myself, these libraries are just better so no need to re-invent the wheel.

  * [Glad](https://github.com/Dav1dde/glad) - OpenGL Loader-Generator
  * [GLFW](https://www.glfw.org/) - For OpenGL and window handling
  * [CGLM](https://github.com/recp/cglm) - Highly optimized graphics math for C
  * [stb_image](https://github.com/nothings/stb) - Image loader
  * [shader.h](https://learnopengl.com/code_viewer_gh.php?code=includes/learnopengl/shader_s.h) - LearnOpenGL's shader class
  * [chicken-scheme](https://www.call-cc.org/) - Enables lisp scripting
  * [assimp][https://www.assimp.org/] - For loading 3d models

## Many thanks
This project wouldn't be possible without these:

  * [Handmade Hero](https://handmadehero.org/) - The project that started my journey to appreciating hand-made programs and applications.
  * [LazyFoo's SDL Tutorials](http://lazyfoo.net/tutorials/SDL/index.php) - I did not use SDL for this engine but I learned about it using these excellent tutorials.
  * [LearnOpenGL](https://learnopengl.com/) - An excellent site containing tutorials about OpenGL. Ramps up from beginner to advanced concepts quite nicely.
  * [Raylib](https://www.raylib.com/) - An excellent game programming library that has very simple and readable code.
  * [Raylib-Chibi](https://github.com/VincentToups/raylib-chibi) - A chibi-scheme implementation for the Raylib library. Scripting on my engine won't be possible without this as a reference.
  * [Myriad Game Engine](https://github.com/jobtalle/Myriad) - A bare bones game engine that I used to learn about OpenGL loading without GLFW.

