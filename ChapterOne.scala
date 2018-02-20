object ChapterOne {

    def fib(n: Int): Int = {
        def compute(current: Int): Int = {
            if (current <= 2) current - 1 
            else compute(current-1) + compute(current-2)
        }
        compute(n)
    }

    def isSorted[A](as: Array[A], ordered: (A, A) => Boolean): Boolean = {
        
        def checkPattern(n: Int): Boolean = {
            if (n + 1 == as.length) {
                ordered(as(n-1), as(n))
            }
            else {
                ordered(as(n), as(n+1)) match {
                    case true => checkPattern(n+1)
                    case false => false
                }
            }
        }

        checkPattern(0)

    }

    def curry[A, B, C](f: (A, B) => C): A => (B => C) = (a: A) => (b: B) => f(a, b)

    def uncurry[A, B, C](f: A => B => C): (A, B) => C =  (a: A, b: B) => f(a)(b)

    def compose[A, B, C](f: B => C, g: A => B): A => C = (a: A) => f(g(a))



}

