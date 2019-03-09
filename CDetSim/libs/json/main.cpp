#include "json.h"

using namespace json;

int main(int argc, char * argv[])
{
    // 1. Create some values
    Value aBool(true);
    Value anInt(34);
    Value aFloat(3.45);
    Value aString("Hello world!");

    // 2. Create an array
    Array ai;
    ai.append(anInt);
    //ai.append(aFloat);
    for (int i = 0; i < 10; ++i) { ai.append(i); }

    for (int i = 0; i < ai.size(); ++i) {
        std::cerr << i << ": " << ai[i].asInt() << '\n';
    }

    // 3. Create an array of arrays
    Array aa;
    for (int j = 0; j < 5; ++j) { ai[0] = 100 + j; aa.append(ai); }
    
    for (int j = 0; j < aa.size(); ++j) {
        Array && a = aa[j].asArray();
        a[1].assign(45 * j);
        for (int i = 0; i < a.size(); ++i) {
            std::cerr << j << ", " << i << ": " << a[i].asInt() << "   ";
        }
        std::cerr << '\n';
    }

    // 4. Create an object with several entities
    Object o1("a", aa);

    o1.append("b", ai);
    o1.append("c", 45);
    o1.append("d", bool(true));
    o1.append("e", std::string("This is a text"));
    
    std::cout << o1 << '\n';

    // 5. Generate the JSON string
    std::stringstream ss;
    ss << o1;
    std::string s1 = ss.str();
    //  std::string s1("{\"key\": \"Hi, world!\", \"key2\": 34}");

    // 6. Parse the generated JSON string into another object
    Object o2;
    Parser p;
    bool isOK = p.parse(s1, o2);
    std::cout << isOK << ": " << o2 << '\n';

    // 7. Create a third object from an input file
    Object o3;
    if (p.parseFile("magic.def.json", o3)) {
	json::enableFormattedOutput("    ");
        std::cout << o3 << '\n';
	json::disableFormattedOutput();
    }

    // 8. Extract part of the object, and set a value
    //Array mirr2 = o3["data"].asObject()["mirrors"].asObject()["value"].asArray()[1].asArray();
    /*
    Array mirr2 = o3["data"]["mirrors"]["value"][1].asArray();
    int n = mirr2.size();
    for (int h = 0; h < n; ++h) { std::cout << mirr2[h] << ' '; }
    std::cout << '\n';

    mirr2[1] = 1101;
    for (int h = 0; h < n; ++h) { std::cout << mirr2[h] << ' '; }
    std::cout << '\n';
    */
    // 9. Test comments support (//, -- and # styles)
    Object o4;
    if (p.parseFile("with_comments.json", o4)) {
	json::enableFormattedOutput("\t");
        std::cout << o4 << '\n';
	json::disableFormattedOutput();
    }

}

