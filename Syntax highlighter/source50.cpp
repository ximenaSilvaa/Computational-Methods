#include <iostream>
#include <vector>
using namespace std;

class Calculator {
public:
    int add(int a, int b) {
        return a + b;
    }
    int subtract(int a, int b) {
        return a - b;
    }
    int multiply(int a, int b) {
        return a * b;
    }
    double divide(double a, double b) {
        if (b != 0)
            return a / b;
        else
            throw "Division by zero!";
    }
};

int main() {
    Calculator calc;
    int a = 20, b = 10;

    cout << "Add: " << calc.add(a, b) << endl;
    cout << "Subtract: " << calc.subtract(a, b) << endl;
    cout << "Multiply: " << calc.multiply(a, b) << endl;

    try {
        cout << "Divide: " << calc.divide(a, b) << endl;
        cout << "Divide by zero: " << calc.divide(a, 0) << endl;
    } catch (const char* msg) {
        cerr << "Error: " << msg << endl;
    }

    vector<int> nums = {1, 2, 3, 4, 5};
    for (int num : nums) {
        cout << num << " ";
    }
    cout << endl;

    int sum = 0;
    for (int i = 0; i < nums.size(); ++i) {
        sum += nums[i];
    }

    cout << "Sum: " << sum << endl;

    return 0;
}
