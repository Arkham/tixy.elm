module Tutorial exposing (Tutorial, all)

import List.Zipper as Zipper exposing (Zipper)


type alias Tutorial =
    { description : String
    , source : String
    }


all : Zipper Tutorial
all =
    Zipper.fromCons firstTutorial otherTutorials


firstTutorial : Tutorial
firstTutorial =
    { description = "tixy - creative code golfing\nclick the dots for more info"
    , source = "sin(y/8*t)"
    }


otherTutorials : List Tutorial
otherTutorials =
    [ { description = "for every dot return 0 or 1\nto change the visibility"
      , source = "1"
      }
    , { description = "use a float between 0 and 1\nto change the size"
      , source = "0.5"
      }
    , { description = "parameter `t` is the\ntime in seconds"
      , source = "sin(t)"
      }
    , { description = "`i` is the index of the dot\nfrom 0 to 255"
      , source = "i / 256"
      }
    , { description = "`x` is the column index\nfrom 0 to 15"
      , source = "x / 16"
      }
    , { description = "`y` is the row index\nalso from 0 to 15"
      , source = "y / 16"
      }
    , { description = "positive numbers are white\nnegative numbers are red"
      , source = "y - 7.5"
      }
    , { description = "use the time parameter\nto animate values"
      , source = "y - t"
      }
    , { description = "multiply the time\nto change the speed"
      , source = "y - t*4"
      }
    , { description = "you can use functions like\nsin, cos, tan, abs and sqrt"
      , source = "sin(t - sqrt((x-7.5)^2 + (y-6)^2))"
      }
    , { description = "combine them with time\nto get cool effects"
      , source = "sin(y/8 + t)"
      }
    , { description = "simple triangle"
      , source = "y - x"
      }
    , { description = "pattern"
      , source = "i%4 - y%4"
      }
    , { description = "mondrian square"
      , source = "(y-6) * (x-6)"
      }
    , { description = "moving cross"
      , source = "(y-4*t|0) * (x-2-t|0)"
      }
    , { description = "sierpinsky"
      , source = "2 * t & i & x & y"
      }
    , { description = "static smooth noise"
      , source = "sin(i ^ 2)"
      }
    , { description = "animated smooth noise"
      , source = "cos(t + i + x * y)"
      }
    , { description = "waves"
      , source = "sin(x/2) - sin(x-t) - y+6"
      }
    , { description = "rotation"
      , source = "sin(2*atan((y-7.5)/(x-7.5))+5*t)"
      }
    , { description = "wipe"
      , source = "(x-y) - sin(t) * 16"
      }
    , { description = "soft wipe"
      , source = "(x-y)/24 - sin(t)"
      }
    , { description = "bloop bloop bloop\nby @v21"
      , source = "(x-8)*(y-8) - sin(t)*64"
      }
    , { description = "fireworks\nby @p_malin and @aemkei"
      , source = "-0.4/(sqrt((x-t%10)^2+(y-t%8)^2)-(t%2)*9)"
      }
    , { description = "ripples\nby @thespite"
      , source = "sin(t-sqrt(x*x+y*y))"
      }
    , { description = "sticky blood\nby @joeytwiddle"
      , source = "y-t*3+9+3*cos(x*3-t)-5*sin(x*7)"
      }
    , { description = "dialogue with an alien\nby @chiptune"
      , source = "1/32*tan(t/64*x*tan(i-x))"
      }
    , { description = "click here to\ncreate your own"
      , source = "HAVE FUN!"
      }
    ]
