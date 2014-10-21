{-
Examples:

28 ++++ TryMe       1737 Jack       [ su 30  20]  22:27 - 23:17 (29-30) W: 16
28 ++++ TryMe       1737 Jack       [ su 30  20]  22:27 - 23:17 (29-30) W: 16
 2 2274 OldManII    ++++ Peshkin    [ bu  2  12]   2:34 -  1:47 (39-39) B:  3
29 1622 Vman        1609 PopKid     [ sr 10  10]   1:14 -  5:10 (21-22) B: 18
32 1880 Raskapov    1859 RoboDweeb  [ br  2  12]   1:04 -  1:26 ( 9-10) B: 34
 1 1878 Roberto     1881 baraka     [psr 45  30]  30:35 - 34:24 (22-22) W: 21


Reading from left to right: (i) game number; (ii) rating of user playing
White; (iii) handle of White, (iv) rating of user playing Black; (v) handle
of Black; (vi) type of match and time controls, (vii) current clock times for
both players; (viii) current material strength for both players, (ix) who is
on move and what move number it will be, and, lastly (x) the number of games
listed in the display and how many in progress. Ratings are ++++ for
unregistered players and ---- for registered players who do not have a rating
in this category. Lastly, any games being examined or setup ('bsetup') are
displayed.

TYPE OF MATCH
-------------

The format will be [Private] | [Category] | [Rated].

Private:  If a "p" is given, the game is private and not open for observation
          (that is "observe" will fail in this case). If the game is not
          private, the space will be blank. (See help variables help file
          concerning how to make a game private.)
Category: The possibilities are:

Type [next] to see next page.
fics% gnext
gnext: Command not found.
fics% next
     b: blitz      l: lightning   u: untimed      e: examined game
     s: standard   w: wild        x: atomic       z: crazyhouse
     B: Bughouse   L: losers      S: Suicide      u: untimed
     n: nonstandard game, such as different time controls

Rated:    The possibilities are "r" for rated and "u" for unrated.


-}


{-
 86 1864 juoni       1944 BradVanHoo [ sr 10  10]   1:09 -  1:15 ( 6- 6) B: 54
 76 1887 cytrus      1931 kubanczykl [ sr 25  10]  19:11 - 14:13 (32-30) W: 18
208 2097 underliined 1731 twobi      [ lr  1   0]   1:00 -  1:00 (39-39) W:  1
233 2091 DoctorBisho 1751 tengiba    [ lr  1   0]   0:54 -  0:57 (38-38) W: 11
 99 1888 betterandbe 1956 PawnsWizar [ sr 15   0]  11:59 - 13:32 (19-17) B: 18
-}
module Game where


data Game = Game { id :: Int
                 , ratingPlayer1 :: Int,
                 , namePlayer1 :: String
                 , ratingPlayer2 :: Int,
                 , namePlayer2 :: String } deriving (Show)








