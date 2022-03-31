
namespace TestUnfoldable {

@test
def test01(): Unit & Impure = 
    let xs: List[Int32] = Unfoldable.unfold(s -> if (s > 10) None else Some(s, s+1), 1);
    xs |> println;
    let c1: Chain[Int32] = Unfoldable.unfold(s -> if (s > 10) None else Some(s, s+1), 1);
    c1 |> println;
    let c2: Chain[Int32] = Unfoldable.replicate(10, 1);
    c2 |> println;
    let c3: Chain[Int32] = Unfoldable.none();
    c3 |> println;
    let c4: Chain[Int32] = Unfoldable.singleton(100);
    c4 |> println;
    let c5: Chain[Int32] = Unfoldable.fromOption(None);
    c5 |> println;
    let c6: Chain[Int32] = Unfoldable.fromOption(Some(0));
    c6 |> println;
    let a1 = ref 0;
    let c7: Chain[Int32] = Unfoldable.unfoldWithIter(_ -> {let ans = deref a1; a1 := deref a1 + 1; if (ans > 4) None else Some(ans)});
    c7 |> println;
    let o1: Option[Char] = Unfoldable.replicate(10, 'b');
    o1 |> println;
    let o2: Option[Char] = Unfoldable.singleton('b');
    o2 |> println;
    ()

}    