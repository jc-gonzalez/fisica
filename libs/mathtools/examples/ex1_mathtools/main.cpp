#include "mathtools.h"
#include "quaternions.h"
#include "surfaces.h"

using namespace MathTools;

#define VEC(x) '(' << x[0] << ", " << x[1] << ", " << x[2] << ')'

int main(int argc, char * argv[])
{
    float Omega[3][3];
    float OmegaI[3][3];

    makeOmega<float>(Omega, d2r<float>(45.), 0.);
    makeOmega<float>(OmegaI, d2r<float>(-45.), 0.);

    float u[3] {1., 4., 8.};
    float v[3];
    float w[3];

    applyMxV<float>(Omega, u, v);
    applyMxV<float>(OmegaI, v, w);

    std::cout << VEC(u)  << "  :  " << VEC(v) << "  :  " << VEC(w) << '\n';

    quaternion q0(1, 2, 3, 4);
    quaternion q1(2, 3, 4, 5);
    quaternion q2(3, 4, 5, 6);
    double r = 7;

    std::cout << "q0:      " << q0 << std::endl;
    std::cout << "q1:      " << q1 << std::endl;
    std::cout << "q2:      " << q2 << std::endl;
    std::cout << "r:       " << r << std::endl;
    std::cout << std::endl;
    std::cout << "-q0:     " << -q0 << std::endl;
    std::cout << "~q0:     " << ~q0 << std::endl;
    std::cout << std::endl;
    std::cout << "r * q0:  " << r*q0 << std::endl;
    std::cout << "r + q0:  " << r+q0 << std::endl;
    std::cout << "q0 / r:  " << q0/r << std::endl;
    std::cout << "q0 - r:  " << q0-r << std::endl;
    std::cout << std::endl;
    std::cout << "q0 + q1: " << q0+q1 << std::endl;
    std::cout << "q0 - q1: " << q0-q1 << std::endl;
    std::cout << "q0 * q1: " << q0*q1 << std::endl;
    std::cout << "q0 / q1: " << q0/q1 << std::endl;
    std::cout << std::endl;
    std::cout << "q0 * ~q0:     " << q0*~q0 << std::endl;
    std::cout << "q0 + q1*q2:   " << q0+q1*q2 << std::endl;
    std::cout << "(q0 + q1)*q2: " << (q0+q1)*q2 << std::endl;
    std::cout << "q0*q1*q2:     " << q0*q1*q2 << std::endl;
    std::cout << "(q0*q1)*q2:   " << (q0*q1)*q2 << std::endl;
    std::cout << "q0*(q1*q2):   " << q0*(q1*q2) << std::endl;
    std::cout << std::endl;
    std::cout << "||q0||:  " << sqrt(q0.norm2()) << std::endl;
    std::cout << std::endl;
    std::cout << "q0*q1 - q1*q0: " << (q0*q1 - q1*q0) << std::endl;

    quaternion ww(1., 1., 1.);
    std::cout << std::endl << ww << " reflected is ";
    ww.reflect(quaternion(0., 0., 0., -1.));
    std::cout << ww << std::endl;

#ifdef COMMENTED_OUT
    sphere s(point3d(0., 0., 8.), 10.);
    line l; l.fromTwoPoints(point3d(100., 100., 1000.),
                            point3d(-5., -5., 0.));
    cylinder c(1.);

    std::vector<point3d> p;
    bool isThereIntersection = intersectionSphereLine(s, l, p);

    if (isThereIntersection) {
        std::cout << "\nPoints of intersection between "
                  << s << " and " << l << ":\n";
        for (auto & pt: p) { std::cout << pt << '\n'; }

        // Lower intersection point is first
        // Try to get the reflection of the vector at that point
        vector3d v_in = p.at(0) - l.o;
        quaternion q_in(v_in, 0.);
        vector3d v_axis = s.c - p.at(0);
        v_axis.normalize();
        quaternion reflec_axis(v_axis, 0.);
        q_in.reflect(reflec_axis);
        vector3d v_out = q_in.getVector();
        std::cout << "Incident vector..: " << v_in << '\n';
        std::cout << "Reflection vector: " << v_out << '\n';

        std::vector<point3d> pp;
        line ll; ll.fromPointVector(p.at(0), v_out);
        bool isThereIntersection2 = intersectionCylinderLine(c, ll, pp);
        if (isThereIntersection2) {
            std::cout << "\nPoints of intersection between "
                      << c << " and " << ll << ":\n";
            for (auto & pt: pp) { std::cout << pt << '\n'; }
        } else {
            std::cout << "No intersection with vertical cylinder!\n";
        }
    } else {
        std::cout << "No intersection!\n";
    }

    std::cout << "\n----------------------------------------\n";
#endif

    // 1 469.195
    //  612.9 98.9922
    // 0.00773524 0.027107 0.999603
    // 85764.6 929468
    // =>
    // 614.545  104.907  218.125   99.948  3.22497  331.179 

    sphere s(point3d(0., 0., 1000.), 1000.);
    line l; l.fromPointVector(point3d(612.9, 98.9922, 0.),
                              point3d(0.00773524, 0.027107, 0.999603));
    cylinder c(100.);

    std::vector<point3d> p;
    bool isThereIntersection = intersectionSphereLine(s, l, p);

    if (isThereIntersection) {
        std::cout << "\nPoints of intersection between "
                  << s << " and " << l << ":\n";
        for (auto & pt: p) { std::cout << pt << '\n'; }

        // Lower intersection point is first
        // Try to get the reflection of the vector at that point
        vector3d v_in = p.at(0) - l.o;
        quaternion q_in(v_in, 0.);
        vector3d v_axis = s.c - p.at(0);
        v_axis.normalize();
        quaternion reflec_axis(v_axis, 0.);
        q_in.reflect(reflec_axis);
        vector3d v_out = q_in.getVector();
        std::cout << "Incident vector..: " << v_in << '\n';
        std::cout << "Reflection vector: " << v_out << '\n';

        std::vector<point3d> pp;
        line ll; ll.fromPointVector(p.at(0), v_out);
        bool isThereIntersection2 = intersectionCylinderLine(c, ll, pp);
        if (isThereIntersection2) {
            std::cout << "\nPoints of intersection between "
                      << c << " and " << ll << ":\n";
            for (auto & pt: pp) { std::cout << pt << '\n'; }
        } else {
            std::cout << "No intersection with vertical cylinder!\n";
        }
    } else {
        std::cout << "No intersection!\n";
    }

    std::cout << "\n----------------------------------------\n";

    matrix3 m;
    m.setRotationThetaPhi(d2r(45.), d2r(0.));
    vector3d vr {1., 0., 1.};
    vector3d vs = m * vr;

    std::cout << vr << "  =>  " << vs << '\n';

    m.setInverseRotationThetaPhi(d2r(45.), d2r(0.));
    vr = m * vs;
    std::cout << vs << "  =>  " << vr << '\n';

    return 0;
}

