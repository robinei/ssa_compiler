//var y = 0
//while y < x do
//    y = y + x
//y

//if x == 1 then
//    10 + x
//else
//    20 + x


var y = 10
if x == 1 then
    y = x + 20
else
    if x == 2 then
        y = x + 30
    else
        y = x + 40
y


//if x == 2 or x == 2 then
//    123
//else
//    666


// 1       1: if x == 1 (1)
//            /      \
// 2: y = x + 20 (1)  3: if x == 2 (1)
//    |               /        \
//    |    5: y = x + 30 (3)   6: y = x + 40 (3)
//    |                    \  /    
//    |                     7: (3)
//    |---------------------/
// 4: ret y (1)
//
