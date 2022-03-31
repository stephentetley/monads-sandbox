


namespace BigFoldLeftM {

def makeList(size: Int32): List[Int32] = 
    List.range(1, size + 1)

@test
def test01(): Unit & Impure =  
    let xs = makeList(10);
    println("foldLeftM...");
    let ans1 = Foldable.foldLeftM((acc, x) -> {println("${x}"); Some(acc+x)}, 0, xs);  
    println("ans1: ${ans1}");
    println("foldRightM...");
    let ans2 = Foldable.foldRightM((x, acc) -> {println("${x}"); Some(acc+x)}, 0, xs);  
    println("ans2: ${ans2}");
    ()

@test
def test02(): Unit & Impure =  
    let xs = makeList(10);
    println("foldLeft...");
    let ans3 = Foldable.foldLeft((acc, x) -> {println("${x}"); acc+x}, 0, xs);  
    println("ans3: ${ans3}");
    println("foldRight...");
    let ans4 = Foldable.foldRight((x, acc) -> {println("${x}"); acc+x}, 0, xs);  
    println("ans4: ${ans4}");
    ()    

}
