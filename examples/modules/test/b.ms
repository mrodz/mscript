type NumberzAreKool int

export class Fizz {
  state: int
  constructor(self, init: NumberzAreKool) {
    self.state = init
  }
}

export type Foo Fizz

type Stool str
export type Bar Stool