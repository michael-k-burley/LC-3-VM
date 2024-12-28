# LC-3 Virtual Machine

### This is a simple LC-3 VM in Scala

#### Intention
I was working through the book **Game Programming Patterns** when I reached chapter 11 on Bytecode. 
It piqued my interest in VMs, so I took a little look online and was fortunate enough to stumble across a fantastic article entitled **Write your Own Virtual Machine**.

I am at least faintly aware of the ridiculousness of writing a VM in a language that itself runs on a VM but here we are. 
This is not a particularly great LC-3 implementation and is in no way pure or functional or anything else Scala-esque. 
But it works, I had quite a bit of fun working on it and I learned a fair bit too.

#### Note

Program can handle binary files as well as hex character files returned by [Web LC3](https://lc3.cs.umanitoba.ca/).

I had to tweak a couple of the LC3 instructions in the assembly files to get them to assemble on the website above.
Specifically, instructions of the form `br #1 ; only execute next line if bit is set` required rewriting. 

Here is a non-exhaustive list of some of the things I found useful and/or learned (mostly the hard way) while working on this project and might have liked to have known about before having begun.

+ Java, and so by extension Scala, doesn't have unsigned primitive integer types.
+ The >> operator preserves the sign (sign-extends), while >>> zeroes the leftmost bits (zero-extends).
+ Any operation on an integer type (+, &, etc.) converts that type to int. ie. promotes/upcasts
+ The first 16 bits of an LC-3 object file specify the address in memory where the program should start. 
  + This address is called the origin. Most commonly set to 0x3000 == #12288 
+ /dev/tty is a special device file, representing the terminal for the current process.
+ Can use stty to print and/or modify line settings for terminals and pseudo-terminals.
+ min N  used with -icanon, will set N characters as the minimum for a completed read.
  + ex. To set 2 chars as minimum needed for full read, use cmd: stty -F /dev/tty -icanon min 2
  + -F is to specify device file. In this case it is: /dev/tty
  + -echo turns off echo. To turn on echo use cmd: stty -F /dev/tty echo
+ ANSI escape sequences beginning with \e will not always be supported.

I left the debug stuff in there cause sometimes it can be useful seeing how someone else did it.

#### How to Use
Program can be ran using scala build tool, with cmd: sbt run

#### Libraries used:
N/A

#### Here is a short list of some resources that I found useful:

+ [Write your Own Virtual Machine](https://www.jmeiners.com/lc3-vm/ "By: Justin Meiners and Ryan Pendleton")
+ [Game Programming Patterns](http://gameprogrammingpatterns.com/ "By Robert Nystrom")
+ [LC-3 Specification](https://www.jmeiners.com/lc3-vm/supplies/lc3-isa.pdf)
+ [x86-64 Introduction](https://github.com/luamfb/intro_x86-64)
+ [ANSI Escape Sequences](https://gist.github.com/fnky/458719343aabd01cfb17a3a4f7296797)
