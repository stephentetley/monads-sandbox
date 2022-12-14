use Indexed/Simple3.{IxState, runState, point, map, flatMap, get, put, modify, liftIO}

def main(): Int32 \ IO = 
    println("run-indexed...");
    demo01() |> println;
    demo02() |> println;
    demo03() |> println;
    0

def demo01(): (String, Int32) = 
    runState(point(1000), "state0")



def demo02(): (String, Int32) \ IO = 
    runState(map(logIt, point(1000)), "state0")

def logIt(x: a): a \ IO with ToString[a] = 
    println("We have seen: ${x}");
    x

def demo03(): (String, Unit) & Impure = 
    runState(logState(), "state0")


def logState(): IxState[s, false, Unit] with ToString[s] = 
    let* s = get();
    let* _ = liftIO(_ -> println("State is: ${s}"));
    point()
