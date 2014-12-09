; quote from [http://wqzhang.wordpress.com/2009/06/18/sicp-exercise-2-16/]
; i think it gives inspirable explanation.
;
; 1. Remember the Taylor expansion: 
; $f(x,y) \approx f(x_0,y_0) + \frac{\partial f}{\partial x} \Delta x + \frac{\partial f}{\partial y} \Delta y$. 
; Maybe we could design a system which utilizes the Taylor expansion, 
; assuming that percentage tolerances are small. To compute the resulting 
; center of an expression, we perform normal arithmetics on the centers of 
; argument intervals. Then we compute the partial derivatives by varying the
; value of one interval while keeping the others fixed. Finally we combine 
; the partial derivatives and percentage tolerances together to obtain the 
; resulting percentage tolerance.
; 2. For really complicated systems, we could also perform Monte carlo 
; simulations to get an answer that is good enough.
