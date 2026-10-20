type First = Second;
type Second = Third;
type Third = First;

type PointerFirst = *PointerSecond;
type PointerSecond = PointerFirst;

type Embedded = Container;
struct Container {
  embedded: Embedded,
}

var value: First;
