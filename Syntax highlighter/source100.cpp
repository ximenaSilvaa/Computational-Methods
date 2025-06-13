#include <iostream>
#include <vector>
#include <string>
using namespace std;

class Student {
private:
    string name;
    int age;
    vector<int> grades;

public:
    Student(string n, int a) : name(n), age(a) {}

    void addGrade(int grade) {
        grades.push_back(grade);
    }

    double average() const {
        if (grades.empty()) return 0.0;
        int sum = 0;
        for (int g : grades) sum += g;
        return (double)sum / grades.size();
    }

    void display() const {
        cout << "Name: " << name << ", Age: " << age << ", Average: " << average() << endl;
    }

    bool operator>(const Student& other) const {
        return this->average() > other.average();
    }
};

class Classroom {
private:
    vector<Student> students;

public:
    void addStudent(const Student& s) {
        students.push_back(s);
    }

    void displayAll() const {
        for (const auto& s : students) {
            s.display();
        }
    }

    void topStudent() const {
        if (students.empty()) {
            cout << "No students." << endl;
            return;
        }
        Student top = students[0];
        for (const auto& s : students) {
            if (s > top) {
                top = s;
            }
        }
        cout << "Top student: ";
        top.display();
    }
};

int main() {
    Classroom classroom;

    Student s1("Alice", 20);
    s1.addGrade(90);
    s1.addGrade(85);
    s1.addGrade(88);

    Student s2("Bob", 22);
    s2.addGrade(75);
    s2.addGrade(80);
    s2.addGrade(79);

    Student s3("Charlie", 21);
    s3.addGrade(92);
    s3.addGrade(87);
    s3.addGrade(90);

    classroom.addStudent(s1);
    classroom.addStudent(s2);
    classroom.addStudent(s3);

    classroom.displayAll();
    classroom.topStudent();

    return 0;
}
