# Rebel Game Engine
A simple game engine using C/C++ built with Scheme scripting. Made mostly from scratch for learning purposes.

The engine is an application of the lessons I've learned researching game engines and poking around various open source projects. I don't plan to use it for production but it might eventually evolve into something that will. The primary goal for now is implementing the barest essentials.

Scripting was initially using [Chibi-Scheme](https://github.com/ashinn/chibi-scheme) but later changed to [Chicken-Scheme](https://www.call-cc.org/). 

## Roadmap
v1.0

- [x] Window management
- [x] Drawing sprites
- [x] Coordinate system
- [x] Scripting Integration
- [ ] Camera System
- [ ] Loading 3D models
- [ ] Sprite transparency
- [ ] Text
- [ ] Audio
- [ ] Save system
- [ ] Multiplatform system
- [ ] C examples
- [ ] Scheme examples

## Dependencies / External Libraries
As much as I want to implement everything myself, these libraries are just better so no need to re-invent the wheel.

  * [Glad](https://github.com/Dav1dde/glad) - OpenGL Loader-Generator
  * [GLFW](https://www.glfw.org/) - For OpenGL and window handling
  * [stb_image](https://github.com/nothings/stb) - Image loader
  * [shader.h](https://learnopengl.com/code_viewer_gh.php?code=includes/learnopengl/shader_s.h) - LearnOpenGL's shader class
  * [chicken-scheme](https://www.call-cc.org/) - An excellent library that allows scripting using the Scheme language

## Many thanks
This project wouldn't be possible without these:

  * [Handmade Hero](https://handmadehero.org/) - The project that started my journey to appreciating hand-made programs and applications.
  * [LazyFoo's SDL Tutorials](http://lazyfoo.net/tutorials/SDL/index.php) - I did not use SDL for this engine but I learned about it using these excellent tutorials.
  * [LearnOpenGL](https://learnopengl.com/) - An excellent site containing tutorials about OpenGL. Ramps up from beginner to advanced concepts quite nicely.
  * [Raylib](https://www.raylib.com/) - An excellent game programming library that has very simple and readable code.
  * [Raylib-Chibi](https://github.com/VincentToups/raylib-chibi) - A chibi-scheme implementation for the Raylib library. Scripting on my engine won't be possible without this as a reference.
  * [Myriad Game Engine](https://github.com/jobtalle/Myriad) - A bare bones game engine that I used to learn about OpenGL loading without GLFW.

