# cl-asteroids
Common Lisp Asteroids Clone

## What is this?
* the idea in this repo was to create a simple asteroids game clone (based on an example from the tic-80 fantasy computer community)
* this was based on my own [cl-game-spell](https://github.com/drigoor/cl-game-spell)
* it is using [raylib](https://www.raylib.com) for drawing and input

### Screenshots

![screenshot000.png](screenshot000.png)

![screenrec000.gif](screenrec000.gif)

---

## Installation instructions and loading (in Windows)

* install [msys2](https://www.msys2.org)

* open a __MinGW64__ shell and do the following:

```bash
pacman -Syu
pacman -Su
pacman -S git
pacman -S --needed base-devel mingw-w64-x86_64-toolchain
pacman -S mingw-w64-x86_64-libffi
pacman -S mingw-w64-x86_64-emacs
```

* install [sbcl](http://www.sbcl.org)

* download [raylib](https://www.raylib.com)

* copy the following .dll to sbcl folder

```text
raylib.dll
libffi-7.dll	<- this one from msys2
```

* ensure that the project is available in the quicklisp local-projects (use a dos cmd window):

```bat
mklink /J c:\home\quicklisp\local-projects\cl-asteroids c:\home\projects\cl-asteroids
```

* load emacs, from the __MinGW64__ shell, (with [slime](https://github.com/slime/slime) or [sly](https://github.com/joaotavora/sly) + [quicklisp](https://www.quicklisp.org/beta/)) and in the lisp repl do:

```cl
(ql:quickload :cl-asteroids)
(asteroids:run)
```

---

#### for running sdl2 (abandonware)

```bash
pacman -S mingw-w64-x86_64-SDL2
pacman -S mingw-w64-x86_64-SDL2_ttf
pacman -S mingw-w64-x86_64-SDL2_image
```

others not installed:
```bash
* mingw-w64-x86_64-SDL2_gfx
* mingw-w64-x86_64-SDL2_mixer
* mingw-w64-x86_64-SDL2_net
* mingw-w64-x86_64-smpeg2
```
