R_utils
=======

Forked from Brendan O'Connor's github repo [dlanalysis][2], Feb 2013.

**util.R contains utilities for R that might be useful.**

From Question [How to organize large R programs?][1] on Stackoverflow.com:

>I like putting different functionality in their own files.
>
>R's package system is rather hard to use.
>
>A lightweight alternative: to place a file's functions inside an environment (what every other language calls a "namespace") and attach it. For example, I made a 'util' group of functions like so:

       util = new.env()
     
       util$bgrep = function [...]
     
       util$timeit = function [...]
     
       while("util" %in% search())
  
            detach("util")
       
       attach(util)
  
>This is all in a file util.R. When you source() it, you get the environment 'util' so you can call util$bgrep() and such; but furthermore, the attach() call makes it so just bgrep() and such work directly. If you didn't put all those functions in their own environment, they'd pollute the interpreter's top-level namespace (the one that ls() shows).
>
>Trying to simulate Python's system, where every file is a module. That would be better to have, but this seems OK.

[1]: http://stackoverflow.com/questions/1266279/how-to-organize-large-r-programs/1319786#1319786
[2]: https://github.com/brendano/dlanalysis
