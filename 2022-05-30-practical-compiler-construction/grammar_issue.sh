#!/bin/sh

# `grammar_issue.md` の状況を再現する
# FsYaccを使って *_good_ast.txt に正しい構文木を生成する
# MyYaccを使って *_bad_ast.txt を生成する (構文エラーになる)

if ! test -f tests/issue_reduce_good_ast.txt
then
    echo '==== UseFsYacc ===='
    cat tests/issue_shift.simple | dotnet run --project UseFsYacc >tests/issue_shift_good_ast.txt
    cat tests/issue_reduce.simple | dotnet run --project UseFsYacc >tests/issue_reduce_good_ast.txt
fi

echo '==== UseMyYacc ===='
cat tests/issue_shift.simple | dotnet run --project Pcc --my-yacc >tests/issue_shift_bad_ast.txt
cat tests/issue_reduce.simple | dotnet run --project Pcc --my-yacc >tests/issue_reduce_bad_ast.txt
