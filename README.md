# Install

    git clone https://github.com/kmels/trac-stats
    cabal install

# Description
    
This command line program downloads a list of events from a Trac Timeline and outputs data that can be plotted with gnuplot.

Future plans: produce a report.

# Usage

As a binary    

    trac-timeline-stats "http://hackage.haskell.org/trac/ghc/timeline?changeset=on&ticket=on&milestone=on&wiki=on&blog=on&max=50&author=&daysback=90&format=rss"

With `runhaskell`

    runhaskell Main.lhs "http://hackage.haskell.org/trac/ghc/timeline?changeset=on&ticket=on&milestone=on&wiki=on&blog=on&max=50&author=&daysback=90&format=rss"

