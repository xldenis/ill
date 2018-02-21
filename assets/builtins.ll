; ModuleID = 'builtins'
source_filename = "<string>"

%Bool = type { i64 }
%Int = type { i64, i64 }
%String = type { i64, [1 x i32] }

declare i8* @malloc(i64)
declare i8* @memcpy(i8*, i8*, i64)

define %Bool* @ltInt(%Int* %a, %Int* %b) {
entry:
  %0 = load %Int, %Int* %a, align 8
  %1 = extractvalue %Int %0, 1

  %2 = load %Int, %Int* %b, align 8
  %3 = extractvalue %Int %2, 1

  %4 = icmp slt i64 %1, %3

  %5 = call i8* @malloc(i64 8)
  %6 = bitcast i8* %5 to i64*

  %7 = sext i1 %4 to i64
  store i64 %7, i64* %6, align 8

  %8 = bitcast i64* %6 to %Bool*
  ret %Bool* %8
}

define %Bool* @gtInt(%Int* %a, %Int* %b) {
entry:
  %0 = load %Int, %Int* %a, align 8
  %1 = extractvalue %Int %0, 1

  %2 = load %Int, %Int* %b, align 8
  %3 = extractvalue %Int %2, 1

  %4 = icmp sgt i64 %1, %3

  %5 = call i8* @malloc(i64 8)
  %6 = bitcast i8* %5 to i64*

  %7 = sext i1 %4 to i64
  store i64 %7, i64* %6, align 8

  %8 = bitcast i64* %6 to %Bool*
  ret %Bool* %8
}

define %Bool* @eqInt(%Int* %a, %Int* %b) {
entry:
  %0 = load %Int, %Int* %a, align 8
  %1 = extractvalue %Int %0, 1

  %2 = load %Int, %Int* %b, align 8
  %3 = extractvalue %Int %2, 1

  %4 = icmp eq i64 %1, %3

  %5 = call i8* @malloc(i64 8)
  %6 = bitcast i8* %5 to i64*

  %7 = sext i1 %4 to i64
  store i64 %7, i64* %6, align 8

  %8 = bitcast i64* %6 to %Bool*
  ret %Bool* %8
}

define %Bool* @geqInt(%Int* %a, %Int* %b) {
entry:
  %0 = load %Int, %Int* %a, align 8
  %1 = extractvalue %Int %0, 1

  %2 = load %Int, %Int* %b, align 8
  %3 = extractvalue %Int %2, 1

  %4 = icmp sge i64 %1, %3

  %5 = call i8* @malloc(i64 8)
  %6 = bitcast i8* %5 to i64*

  %7 = sext i1 %4 to i64
  store i64 %7, i64* %6, align 8

  %8 = bitcast i64* %6 to %Bool*
  ret %Bool* %8
}

define %Bool* @leqInt(%Int* %a, %Int* %b) {
entry:
  %0 = load %Int, %Int* %a, align 8
  %1 = extractvalue %Int %0, 1

  %2 = load %Int, %Int* %b, align 8
  %3 = extractvalue %Int %2, 1

  %4 = icmp sle i64 %1, %3

  %5 = call i8* @malloc(i64 8)
  %6 = bitcast i8* %5 to i64*

  %7 = sext i1 %4 to i64
  store i64 %7, i64* %6, align 8

  %8 = bitcast i64* %6 to %Bool*
  ret %Bool* %8
}

