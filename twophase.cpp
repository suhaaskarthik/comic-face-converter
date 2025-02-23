#include<iostream>
#include<vector>
#include<limits>
#include<map>
using namespace std;
const double epsilon = 1e-15;

class tphase{
    public:
    int num,con,l,g,e;
    vector<pair<int,int> >arti;
    vector<vector<double> >table;
    map<int, int> slack, surplus;
    bool maxi;
    int col;
    vector<int> obj;
    tphase(int n,int c,int ls,int gs,int es){
        num = n;
        con = c;
        l = ls;
        g = gs;
        e = es;
        col = num;
        maxi = true;
        table.resize(con + 1, vector<double>(num + l + (2*g) + e+1, 0)); // Correct table allocation
    }
    // Adding the constraints
    void add(vector<int> vals, int a, int row) {
        if (a == 0) { // Equality
            for (int i = 0; i < vals.size() - 1; i++) {
                table[row][i] = vals[i];
            }
            table[row][col] = 1;
            arti.push_back(make_pair(col,row));
            col++;
            table[row][table[row].size() - 1] = vals[vals.size() - 1];
        } else if (a == 1) { // Less than
            for (int i = 0; i < vals.size() - 1; i++) {
                table[row][i] = vals[i];
            }
            table[row][col] = 1; // Fixing slack variable index
            slack[col] = row;
            col++; // Increment column index
            table[row][table[row].size() - 1] = vals[vals.size() - 1];
        } else {
            for (int i = 0; i < vals.size() - 1; i++) {
                table[row][i] = vals[i];
            }
            table[row][col] = -1; // Fixing surplus variable index
            surplus[col] = row;
            col++; // Increment column index
            table[row][col] = 1;
            arti.push_back(make_pair(col,row));
            col++;
            table[row][table[row].size() - 1] = vals[vals.size() - 1];
        }
    }
    
    void set(vector<int>vals,int a){
         obj = vals;
         maxi = (a==1);
         for(auto i:arti){
            table[con][i.first] = +1;
         }
         print();
         for(auto i:arti){
            int row = -1;
            for(int j = 0;j<table.size()-1;j++){
                if(table[j][i.first]==1){
                    row = j;
                    break;
                }
            }
            for(int j = 0;j<table[0].size();j++){
                table[con][j] -= table[row][j];
            }
        }
    }
    // Finding pivot column (most negative coefficient in last row)
    int pivotcol() {
        int col = -1;
        double mini = 0;
        for (int i = 0; i < table[con].size() - 1; i++) { // Avoid RHS column
            if (table[con][i] < mini) {
                mini = table[con][i];
                col = i;
            }
        }
        return col;
    }

    // Finding pivot row (smallest positive ratio)
    int pivotrow() {
        int row = -1;
        double mini = numeric_limits<double>::max();
        int a = pivotcol();
        for (int i = 0; i < con; i++) {
            if (table[i][a] != 0) { // Prevent division by zero
                double r = table[i][table[i].size() - 1] / table[i][a];
                if (r > 0 && r < mini) {
                    mini = r;
                    row = i;
                }
            }
        }
        return row;
    }

    // Performing the pivot operation
    void pivot(int col, int row) {
        double p = table[row][col];

        // Normalize the pivot row
        for (int i = 0; i < table[row].size(); i++) {
            table[row][i] /= p;
        }

        // Reduce other rows
        for (int i = 0; i < table.size(); i++) {
            if (i != row) {
                double factor = table[i][col];
                for (int j = 0; j < table[i].size(); j++) {
                    table[i][j] -= factor * table[row][j]; // Fixing incorrect updates
                }
            }
        }
    }

    // Solving the simplex tableau
    void solve() {
        bool flag = true;
        while (true) {
            print();
            int col = pivotcol();
            if (col == -1) {
                cout << "Phase 1 done.\n";
                break;
            }
            int row = pivotrow();
            if (row == -1) {
                cout << "Infeasible solution.\n";
                flag = false;
                break;
            }
            pivot(col, row);
        }
        print();
        if(table[con][table[0].size()-1]!=0){
            cout << "Infeasible solution.\n";
            return;
        }
        if (flag) {
          if (!maxi) { // Minimize
            for (int i = 0; i < obj.size(); i++) {
                obj[i] = -obj[i];
            }
        }
        vector<vector<double> >vals;
        vals.resize(con+1);
        for(int i = 0;i<table.size();i++){
            for(int j = 0;j<table[0].size();j++){
                bool ch = true;
                for(auto k: arti){
                    if(k.first == j){
                        ch = false;
                    }
                }
                if(ch){
                    vals[i].push_back(table[i][j]);
                }else{
                    continue;
                }
            }
        }
        table = vals;
        for (int i = 0; i < obj.size() - 1; i++) {
            table[con][i] = -obj[i];
        }
        print();
        vector<int>v = basic();
        for(auto i:v){
            int row = -1;
            for(int j = 0;j<table.size()-1;j++){
                if(table[j][i]==1){
                    row = j;
                    break;
                }
            }
            double p= table[con][i];
            for(int j = 0;j<table[0].size();j++){
                table[con][j] -= p*table[row][j];
            }
        }
        }
        flag = true;
        while (true) {
            print();
            int col = pivotcol();
            if (col == -1) {
                cout << "Optimum solution found.\n";
                break;
            }
            int row = pivotrow();
            if (row == -1) {
                cout << "Infeasible solution.\n";
                flag = false;
                break;
            }
            pivot(col, row);
        }
        print();
        if (flag) {
            for (int i = 0; i < table[0].size() - 1; i++) {
                double val = 0, temp = 0;
                int count = 0;
                bool f= true;
                for (int j = 0; j < con; j++) {
                    if (table[j][i] == 1) {
                        count++;
                        temp = table[j][table[0].size() - 1];
                    }else if(table[j][i] != 0){
                        f = false;
                    }
                }
                if (count == 1 && f) {
                    val = temp;}
                    cout << "x" << i << ": " << val << "\n";
            }
            if (maxi) {
                cout << "Max value: " << table[con][table[0].size() - 1] << "\n";
            } else {
                cout << "Min value: " << -table[con][table[0].size() - 1] << "\n";
            }
        }
    }
    void print() {
        cout << "Simplex Tableau:\n";
        for (int i = 0; i < table.size(); i++) {
            for (int j = 0; j < table[i].size(); j++) {
                if(table[i][j]>=-epsilon && table[i][j]<=epsilon){
                    table[i][j] = 0;
                }
                cout << table[i][j] << " ";
            }
            cout << "\n";
        }
        cout << "\n";
    }

    vector<int> basic(){
        vector<int>vals;
        for (int i = 0; i < table[0].size() - 1; i++) {
                double val = 0, temp = 0;
                int count = 0;
                bool f= true;
                for (int j = 0; j < con; j++) {
                    if (table[j][i] == 1) {
                        count++;
                        temp = table[j][table[0].size() - 1];
                    }else if(table[j][i] != 0){
                        f = false;
                    }
                }
                if(count == 1&& f){
                    vals.push_back(i);
                }
            }
        return vals;
    }

};

int main() {
    int num, con, countl, counte, countg;

    cout << "Enter the number of variables, constraints, less than constraints, equality constraints, greater than constraints:\n" << flush;
    cin >> num >> con >> countl >> counte >> countg;

    tphase model(num, con, countl, countg, counte);

    for (int i = 0; i < con; i++) {
        cout << "Enter constraint " << i + 1 << "\n" << flush;
        vector<int> vals(num + 1);

        for (int j = 0; j < num; j++) {
            cout << "Enter coefficient of x" << j + 1 << ": " << flush;
            cin >> vals[j];
        }

        cout << "Enter RHS of constraint: " << flush;
        cin >> vals[num];

        int k;
        cout << "Enter constraint type (0 = equality, 1 = less than, 2 = greater than): " << flush;
        cin >> k;

        model.add(vals, k, i);
        model.print();
    }

    vector<int> obj(num + 1);
    cout << "Enter objective function coefficients:\n" << flush;
    for (int j = 0; j < num; j++) {
        cout << "Enter coefficient of x" << j + 1 << ": " << flush;
        cin >> obj[j];
    }

    int optType;
    cout << "Enter 1 for maximization, 0 for minimization: " << flush;
    cin >> optType;

    model.set(obj, optType);
    model.solve();

    return 0;
}
