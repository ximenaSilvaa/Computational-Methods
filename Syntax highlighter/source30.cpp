#include <iostream>
using namespace std;

struct Point {
    int x, y;
    Point(int x_val, int y_val) : x(x_val), y(y_val) {}
};

class Rectangle {
private:
    Point p1, p2;
public:
    Rectangle(Point a, Point b) : p1(a), p2(b) {}

    int area() const {
        return (p2.x - p1.x) * (p2.y - p1.y);
    }

    void display() const {
        cout << "Area: " << area() << endl;
    }
};

int main() {
    Point topLeft(0, 10);
    Point bottomRight(5, 0);
    Rectangle rect(topLeft, bottomRight);

    rect.display();

    if (rect.area() > 0) {
        cout << "Valid rectangle" << endl;
    } else {
        cout << "Invalid rectangle" << endl;
    }

    return 0;
}