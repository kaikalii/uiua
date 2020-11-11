( list(a) a -- list(a) )
( list(list(a)) list(a) -- list(list(a)) )
( list(list(a)) list(a) a -- list(list(a)) )

( b a -- b )    [ b a ]     [ b ]
          ( c b -- c )    [ c b ]     [ c ]
( c b a -- c )  [ c b a ]   [ c ]

( a a -- a )
( list(a) a -- list(a) )
( list(a) a a -- list(a) )

( a a -- a )    [ a a ]     [ a ]
          ( b a -- b )    [ b a ]     [ b ]
( b a a -- b )  [ b a a ]   [ b ]

( a a -- a )        [ a a ] [ a ]
        ( list(a) -- a )    [ b ]   [ a ]
error

( -- a ) [] [ a ]
     ( -- b ) [] [ b ]
( -- a b ) [] [ a b ]