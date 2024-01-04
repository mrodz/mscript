export class Point {
    x: int
    y: int
    constructor(self, x: int, y: int) {
        self.x = x
        self.y = y
    }
}

export add_points: fn(Point, Point) -> Point = fn(p1: Point, p2: Point) -> Point {
    return Point(p1.x + p2.x, p1.y + p2.y)
}