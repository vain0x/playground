void inc(int inc_ret, int* x) {
    int KAddPrim = x + 1;
    int KAssignPrim = x = KAddPrim;
    inc_ret(KAssignPrim);
}

void assert_ret_3(int assert_res_3) {
    main_ret_2(assert_res_3);
}

void inc_ret_3(int* inc_res_2) {
    int KEqPrim_3 = a == 3;
    assert(assert_ret_3, KEqPrim_3);
}

void assert_ret_2(int assert_res_2) {
    inc(inc_ret_3, &a);
}

void inc_ret_2(int* inc_res) {
    int KEqPrim_2 = a == 2;
    assert(assert_ret_2, KEqPrim_2);
}

void assert_ret(int assert_res) {
    inc(inc_ret_2, &a);
}

void a_next(int a) {
    int KEqPrim = a == 1;
    assert(assert_ret, KEqPrim);
}

void main(int main_ret_2) {
    a_next(1);
}
