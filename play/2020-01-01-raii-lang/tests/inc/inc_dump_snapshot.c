void inc(int inc_ret, int* x) {
    int KAddPrim = x + 1;
    int KAssignPrim = x = KAddPrim;
    inc_ret(KAssignPrim);
}

void inc_ret_3(int* inc_res_2, int* main_ret) {
    main_ret(inc_res_2);
}

void inc_ret_2(int* inc_res, int* a, int* main_ret) {
    inc(inc_ret_3, &a);
}

void a_next(int a, int* main_ret) {
    inc(inc_ret_2, &a);
}

void main(int main_ret) {
    a_next(1, &main_ret);
}
