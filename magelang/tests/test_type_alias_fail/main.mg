type First = Second;
type Second = Third;
type Third = First;

type PointerFirst = *PointerSecond;
type PointerSecond = PointerFirst;

type Embedded = Container;
type Container struct {
  embedded: Embedded,
}

let value: First;
