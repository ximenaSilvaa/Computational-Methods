#include <iostream>
using namespace std;

class MyClass {
public:
    int value;
    MyClass(int val) : value(val) {}
    void display() const {
        cout << "Value: " << value << endl;
    }
};

int main() {
    MyClass obj(42);
    obj.display();

    if (obj.value > 0) {
        cout << "Positive value" << endl;
    } else {
        cout << "Non-positive value" << endl;
    }

    return 0;
}
