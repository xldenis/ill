; ModuleID = 'rts.c'
source_filename = "rts.c"
target datalayout = "e-m:o-i64:64-f80:128-n8:16:32:64-S128"
target triple = "x86_64-apple-macosx10.13.0"

%struct.String = type { i64, i8* }
%struct.Int = type { i64 }
%struct.Bool = type { i64 }
%struct.Double = type { double }

@.str = private unnamed_addr constant [5 x i8] c"%s%s\00", align 1
@.str.1 = private unnamed_addr constant [5 x i8] c"%lld\00", align 1
@.str.2 = private unnamed_addr constant [3 x i8] c"%f\00", align 1
@.str.3 = private unnamed_addr constant [4 x i8] c"%s\0A\00", align 1

; Function Attrs: noinline nounwind optnone ssp uwtable
define %struct.String* @mkString(i64) #0 {
  %2 = alloca i64, align 8
  %3 = alloca %struct.String*, align 8
  %4 = alloca i8*, align 8
  store i64 %0, i64* %2, align 8
  %5 = call i8* @malloc(i64 16) #4
  %6 = bitcast i8* %5 to %struct.String*
  store %struct.String* %6, %struct.String** %3, align 8
  %7 = load i64, i64* %2, align 8
  %8 = add i64 %7, 1
  %9 = mul i64 %8, 1
  %10 = call i8* @malloc(i64 %9) #4
  store i8* %10, i8** %4, align 8
  %11 = load i64, i64* %2, align 8
  %12 = load %struct.String*, %struct.String** %3, align 8
  %13 = getelementptr inbounds %struct.String, %struct.String* %12, i32 0, i32 0
  store i64 %11, i64* %13, align 8
  %14 = load i8*, i8** %4, align 8
  %15 = load %struct.String*, %struct.String** %3, align 8
  %16 = getelementptr inbounds %struct.String, %struct.String* %15, i32 0, i32 1
  store i8* %14, i8** %16, align 8
  %17 = load %struct.String*, %struct.String** %3, align 8
  ret %struct.String* %17
}

; Function Attrs: allocsize(0)
declare i8* @malloc(i64) #1

; Function Attrs: noinline nounwind optnone ssp uwtable
define %struct.String* @plusStr(%struct.String*, %struct.String*) #0 {
  %3 = alloca %struct.String*, align 8
  %4 = alloca %struct.String*, align 8
  %5 = alloca i32, align 4
  %6 = alloca %struct.String*, align 8
  store %struct.String* %0, %struct.String** %3, align 8
  store %struct.String* %1, %struct.String** %4, align 8
  %7 = load %struct.String*, %struct.String** %3, align 8
  %8 = getelementptr inbounds %struct.String, %struct.String* %7, i32 0, i32 0
  %9 = load i64, i64* %8, align 8
  %10 = load %struct.String*, %struct.String** %4, align 8
  %11 = getelementptr inbounds %struct.String, %struct.String* %10, i32 0, i32 0
  %12 = load i64, i64* %11, align 8
  %13 = add i64 %9, %12
  %14 = trunc i64 %13 to i32
  store i32 %14, i32* %5, align 4
  %15 = load i32, i32* %5, align 4
  %16 = sext i32 %15 to i64
  %17 = call %struct.String* @mkString(i64 %16)
  store %struct.String* %17, %struct.String** %6, align 8
  %18 = load %struct.String*, %struct.String** %6, align 8
  %19 = getelementptr inbounds %struct.String, %struct.String* %18, i32 0, i32 1
  %20 = load i8*, i8** %19, align 8
  %21 = load i32, i32* %5, align 4
  %22 = add nsw i32 %21, 1
  %23 = sext i32 %22 to i64
  %24 = load %struct.String*, %struct.String** %6, align 8
  %25 = getelementptr inbounds %struct.String, %struct.String* %24, i32 0, i32 1
  %26 = load i8*, i8** %25, align 8
  %27 = call i64 @llvm.objectsize.i64.p0i8(i8* %26, i1 false, i1 true)
  %28 = load %struct.String*, %struct.String** %3, align 8
  %29 = getelementptr inbounds %struct.String, %struct.String* %28, i32 0, i32 1
  %30 = load i8*, i8** %29, align 8
  %31 = load %struct.String*, %struct.String** %4, align 8
  %32 = getelementptr inbounds %struct.String, %struct.String* %31, i32 0, i32 1
  %33 = load i8*, i8** %32, align 8
  %34 = call i32 (i8*, i64, i32, i64, i8*, ...) @__snprintf_chk(i8* %20, i64 %23, i32 0, i64 %27, i8* getelementptr inbounds ([5 x i8], [5 x i8]* @.str, i32 0, i32 0), i8* %30, i8* %33)
  %35 = load %struct.String*, %struct.String** %6, align 8
  ret %struct.String* %35
}

declare i32 @__snprintf_chk(i8*, i64, i32, i64, i8*, ...) #2

; Function Attrs: nounwind readnone speculatable
declare i64 @llvm.objectsize.i64.p0i8(i8*, i1, i1) #3

; Function Attrs: noinline nounwind optnone ssp uwtable
define %struct.String* @showInt(%struct.Int*) #0 {
  %2 = alloca %struct.Int*, align 8
  %3 = alloca i32, align 4
  %4 = alloca %struct.String*, align 8
  store %struct.Int* %0, %struct.Int** %2, align 8
  %5 = call i64 @llvm.objectsize.i64.p0i8(i8* null, i1 false, i1 true)
  %6 = load %struct.Int*, %struct.Int** %2, align 8
  %7 = getelementptr inbounds %struct.Int, %struct.Int* %6, i32 0, i32 0
  %8 = load i64, i64* %7, align 8
  %9 = call i32 (i8*, i64, i32, i64, i8*, ...) @__snprintf_chk(i8* null, i64 0, i32 0, i64 %5, i8* getelementptr inbounds ([5 x i8], [5 x i8]* @.str.1, i32 0, i32 0), i64 %8)
  %10 = add nsw i32 %9, 1
  store i32 %10, i32* %3, align 4
  %11 = load i32, i32* %3, align 4
  %12 = sext i32 %11 to i64
  %13 = call %struct.String* @mkString(i64 %12)
  store %struct.String* %13, %struct.String** %4, align 8
  %14 = load %struct.String*, %struct.String** %4, align 8
  %15 = getelementptr inbounds %struct.String, %struct.String* %14, i32 0, i32 1
  %16 = load i8*, i8** %15, align 8
  %17 = load i32, i32* %3, align 4
  %18 = sext i32 %17 to i64
  %19 = load %struct.String*, %struct.String** %4, align 8
  %20 = getelementptr inbounds %struct.String, %struct.String* %19, i32 0, i32 1
  %21 = load i8*, i8** %20, align 8
  %22 = call i64 @llvm.objectsize.i64.p0i8(i8* %21, i1 false, i1 true)
  %23 = load %struct.Int*, %struct.Int** %2, align 8
  %24 = getelementptr inbounds %struct.Int, %struct.Int* %23, i32 0, i32 0
  %25 = load i64, i64* %24, align 8
  %26 = call i32 (i8*, i64, i32, i64, i8*, ...) @__snprintf_chk(i8* %16, i64 %18, i32 0, i64 %22, i8* getelementptr inbounds ([5 x i8], [5 x i8]* @.str.1, i32 0, i32 0), i64 %25)
  %27 = load %struct.String*, %struct.String** %4, align 8
  ret %struct.String* %27
}

; Function Attrs: noinline nounwind optnone ssp uwtable
define %struct.Int* @plusInt(%struct.Int*, %struct.Int*) #0 {
  %3 = alloca %struct.Int*, align 8
  %4 = alloca %struct.Int*, align 8
  store %struct.Int* %0, %struct.Int** %3, align 8
  store %struct.Int* %1, %struct.Int** %4, align 8
  %5 = load %struct.Int*, %struct.Int** %3, align 8
  %6 = getelementptr inbounds %struct.Int, %struct.Int* %5, i32 0, i32 0
  %7 = load i64, i64* %6, align 8
  %8 = load %struct.Int*, %struct.Int** %4, align 8
  %9 = getelementptr inbounds %struct.Int, %struct.Int* %8, i32 0, i32 0
  %10 = load i64, i64* %9, align 8
  %11 = add i64 %7, %10
  %12 = call %struct.Int* @mkInt(i64 %11)
  ret %struct.Int* %12
}

declare %struct.Int* @mkInt(i64) #2

; Function Attrs: noinline nounwind optnone ssp uwtable
define %struct.Int* @minusInt(%struct.Int*, %struct.Int*) #0 {
  %3 = alloca %struct.Int*, align 8
  %4 = alloca %struct.Int*, align 8
  store %struct.Int* %0, %struct.Int** %3, align 8
  store %struct.Int* %1, %struct.Int** %4, align 8
  %5 = load %struct.Int*, %struct.Int** %3, align 8
  %6 = getelementptr inbounds %struct.Int, %struct.Int* %5, i32 0, i32 0
  %7 = load i64, i64* %6, align 8
  %8 = load %struct.Int*, %struct.Int** %4, align 8
  %9 = getelementptr inbounds %struct.Int, %struct.Int* %8, i32 0, i32 0
  %10 = load i64, i64* %9, align 8
  %11 = sub i64 %7, %10
  %12 = call %struct.Int* @mkInt(i64 %11)
  ret %struct.Int* %12
}

; Function Attrs: noinline nounwind optnone ssp uwtable
define %struct.Bool* @ltInt(%struct.Int*, %struct.Int*) #0 {
  %3 = alloca %struct.Int*, align 8
  %4 = alloca %struct.Int*, align 8
  store %struct.Int* %0, %struct.Int** %3, align 8
  store %struct.Int* %1, %struct.Int** %4, align 8
  %5 = load %struct.Int*, %struct.Int** %3, align 8
  %6 = getelementptr inbounds %struct.Int, %struct.Int* %5, i32 0, i32 0
  %7 = load i64, i64* %6, align 8
  %8 = load %struct.Int*, %struct.Int** %4, align 8
  %9 = getelementptr inbounds %struct.Int, %struct.Int* %8, i32 0, i32 0
  %10 = load i64, i64* %9, align 8
  %11 = icmp ult i64 %7, %10
  br i1 %11, label %12, label %14

; <label>:12:                                     ; preds = %2
  %13 = call %struct.Bool* (...) @True()
  br label %16

; <label>:14:                                     ; preds = %2
  %15 = call %struct.Bool* (...) @False()
  br label %16

; <label>:16:                                     ; preds = %14, %12
  %17 = phi %struct.Bool* [ %13, %12 ], [ %15, %14 ]
  ret %struct.Bool* %17
}

declare %struct.Bool* @True(...) #2

declare %struct.Bool* @False(...) #2

; Function Attrs: noinline nounwind optnone ssp uwtable
define %struct.Bool* @gtInt(%struct.Int*, %struct.Int*) #0 {
  %3 = alloca %struct.Int*, align 8
  %4 = alloca %struct.Int*, align 8
  store %struct.Int* %0, %struct.Int** %3, align 8
  store %struct.Int* %1, %struct.Int** %4, align 8
  %5 = load %struct.Int*, %struct.Int** %3, align 8
  %6 = getelementptr inbounds %struct.Int, %struct.Int* %5, i32 0, i32 0
  %7 = load i64, i64* %6, align 8
  %8 = load %struct.Int*, %struct.Int** %4, align 8
  %9 = getelementptr inbounds %struct.Int, %struct.Int* %8, i32 0, i32 0
  %10 = load i64, i64* %9, align 8
  %11 = icmp ugt i64 %7, %10
  br i1 %11, label %12, label %14

; <label>:12:                                     ; preds = %2
  %13 = call %struct.Bool* (...) @True()
  br label %16

; <label>:14:                                     ; preds = %2
  %15 = call %struct.Bool* (...) @False()
  br label %16

; <label>:16:                                     ; preds = %14, %12
  %17 = phi %struct.Bool* [ %13, %12 ], [ %15, %14 ]
  ret %struct.Bool* %17
}

; Function Attrs: noinline nounwind optnone ssp uwtable
define %struct.Bool* @eqInt(%struct.Int*, %struct.Int*) #0 {
  %3 = alloca %struct.Int*, align 8
  %4 = alloca %struct.Int*, align 8
  store %struct.Int* %0, %struct.Int** %3, align 8
  store %struct.Int* %1, %struct.Int** %4, align 8
  %5 = load %struct.Int*, %struct.Int** %3, align 8
  %6 = getelementptr inbounds %struct.Int, %struct.Int* %5, i32 0, i32 0
  %7 = load i64, i64* %6, align 8
  %8 = load %struct.Int*, %struct.Int** %4, align 8
  %9 = getelementptr inbounds %struct.Int, %struct.Int* %8, i32 0, i32 0
  %10 = load i64, i64* %9, align 8
  %11 = icmp eq i64 %7, %10
  br i1 %11, label %12, label %14

; <label>:12:                                     ; preds = %2
  %13 = call %struct.Bool* (...) @True()
  br label %16

; <label>:14:                                     ; preds = %2
  %15 = call %struct.Bool* (...) @False()
  br label %16

; <label>:16:                                     ; preds = %14, %12
  %17 = phi %struct.Bool* [ %13, %12 ], [ %15, %14 ]
  ret %struct.Bool* %17
}

; Function Attrs: noinline nounwind optnone ssp uwtable
define %struct.Bool* @leqInt(%struct.Int*, %struct.Int*) #0 {
  %3 = alloca %struct.Int*, align 8
  %4 = alloca %struct.Int*, align 8
  store %struct.Int* %0, %struct.Int** %3, align 8
  store %struct.Int* %1, %struct.Int** %4, align 8
  %5 = load %struct.Int*, %struct.Int** %3, align 8
  %6 = getelementptr inbounds %struct.Int, %struct.Int* %5, i32 0, i32 0
  %7 = load i64, i64* %6, align 8
  %8 = load %struct.Int*, %struct.Int** %4, align 8
  %9 = getelementptr inbounds %struct.Int, %struct.Int* %8, i32 0, i32 0
  %10 = load i64, i64* %9, align 8
  %11 = icmp ule i64 %7, %10
  br i1 %11, label %12, label %14

; <label>:12:                                     ; preds = %2
  %13 = call %struct.Bool* (...) @True()
  br label %16

; <label>:14:                                     ; preds = %2
  %15 = call %struct.Bool* (...) @False()
  br label %16

; <label>:16:                                     ; preds = %14, %12
  %17 = phi %struct.Bool* [ %13, %12 ], [ %15, %14 ]
  ret %struct.Bool* %17
}

; Function Attrs: noinline nounwind optnone ssp uwtable
define %struct.Bool* @geqInt(%struct.Int*, %struct.Int*) #0 {
  %3 = alloca %struct.Int*, align 8
  %4 = alloca %struct.Int*, align 8
  store %struct.Int* %0, %struct.Int** %3, align 8
  store %struct.Int* %1, %struct.Int** %4, align 8
  %5 = load %struct.Int*, %struct.Int** %3, align 8
  %6 = getelementptr inbounds %struct.Int, %struct.Int* %5, i32 0, i32 0
  %7 = load i64, i64* %6, align 8
  %8 = load %struct.Int*, %struct.Int** %4, align 8
  %9 = getelementptr inbounds %struct.Int, %struct.Int* %8, i32 0, i32 0
  %10 = load i64, i64* %9, align 8
  %11 = icmp uge i64 %7, %10
  br i1 %11, label %12, label %14

; <label>:12:                                     ; preds = %2
  %13 = call %struct.Bool* (...) @True()
  br label %16

; <label>:14:                                     ; preds = %2
  %15 = call %struct.Bool* (...) @False()
  br label %16

; <label>:16:                                     ; preds = %14, %12
  %17 = phi %struct.Bool* [ %13, %12 ], [ %15, %14 ]
  ret %struct.Bool* %17
}

; Function Attrs: noinline nounwind optnone ssp uwtable
define %struct.String* @showDouble(%struct.Double*) #0 {
  %2 = alloca %struct.Double*, align 8
  %3 = alloca i32, align 4
  %4 = alloca %struct.String*, align 8
  store %struct.Double* %0, %struct.Double** %2, align 8
  %5 = call i64 @llvm.objectsize.i64.p0i8(i8* null, i1 false, i1 true)
  %6 = load %struct.Double*, %struct.Double** %2, align 8
  %7 = getelementptr inbounds %struct.Double, %struct.Double* %6, i32 0, i32 0
  %8 = load double, double* %7, align 8
  %9 = call i32 (i8*, i64, i32, i64, i8*, ...) @__snprintf_chk(i8* null, i64 0, i32 0, i64 %5, i8* getelementptr inbounds ([3 x i8], [3 x i8]* @.str.2, i32 0, i32 0), double %8)
  %10 = add nsw i32 %9, 1
  store i32 %10, i32* %3, align 4
  %11 = load i32, i32* %3, align 4
  %12 = sext i32 %11 to i64
  %13 = call %struct.String* @mkString(i64 %12)
  store %struct.String* %13, %struct.String** %4, align 8
  %14 = load %struct.String*, %struct.String** %4, align 8
  %15 = getelementptr inbounds %struct.String, %struct.String* %14, i32 0, i32 1
  %16 = load i8*, i8** %15, align 8
  %17 = load i32, i32* %3, align 4
  %18 = sext i32 %17 to i64
  %19 = load %struct.String*, %struct.String** %4, align 8
  %20 = getelementptr inbounds %struct.String, %struct.String* %19, i32 0, i32 1
  %21 = load i8*, i8** %20, align 8
  %22 = call i64 @llvm.objectsize.i64.p0i8(i8* %21, i1 false, i1 true)
  %23 = load %struct.Double*, %struct.Double** %2, align 8
  %24 = getelementptr inbounds %struct.Double, %struct.Double* %23, i32 0, i32 0
  %25 = load double, double* %24, align 8
  %26 = call i32 (i8*, i64, i32, i64, i8*, ...) @__snprintf_chk(i8* %16, i64 %18, i32 0, i64 %22, i8* getelementptr inbounds ([3 x i8], [3 x i8]* @.str.2, i32 0, i32 0), double %25)
  %27 = load %struct.String*, %struct.String** %4, align 8
  ret %struct.String* %27
}

; Function Attrs: noinline nounwind optnone ssp uwtable
define %struct.Double* @plusDouble(%struct.Double*, %struct.Double*) #0 {
  %3 = alloca %struct.Double*, align 8
  %4 = alloca %struct.Double*, align 8
  store %struct.Double* %0, %struct.Double** %3, align 8
  store %struct.Double* %1, %struct.Double** %4, align 8
  %5 = load %struct.Double*, %struct.Double** %3, align 8
  %6 = getelementptr inbounds %struct.Double, %struct.Double* %5, i32 0, i32 0
  %7 = load double, double* %6, align 8
  %8 = load %struct.Double*, %struct.Double** %4, align 8
  %9 = getelementptr inbounds %struct.Double, %struct.Double* %8, i32 0, i32 0
  %10 = load double, double* %9, align 8
  %11 = fadd double %7, %10
  %12 = call %struct.Double* @mkDouble(double %11)
  ret %struct.Double* %12
}

declare %struct.Double* @mkDouble(double) #2

; Function Attrs: noinline nounwind optnone ssp uwtable
define %struct.Double* @minusDouble(%struct.Double*, %struct.Double*) #0 {
  %3 = alloca %struct.Double*, align 8
  %4 = alloca %struct.Double*, align 8
  store %struct.Double* %0, %struct.Double** %3, align 8
  store %struct.Double* %1, %struct.Double** %4, align 8
  %5 = load %struct.Double*, %struct.Double** %3, align 8
  %6 = getelementptr inbounds %struct.Double, %struct.Double* %5, i32 0, i32 0
  %7 = load double, double* %6, align 8
  %8 = load %struct.Double*, %struct.Double** %4, align 8
  %9 = getelementptr inbounds %struct.Double, %struct.Double* %8, i32 0, i32 0
  %10 = load double, double* %9, align 8
  %11 = fsub double %7, %10
  %12 = call %struct.Double* @mkDouble(double %11)
  ret %struct.Double* %12
}

; Function Attrs: noinline nounwind optnone ssp uwtable
define %struct.Bool* @ltDouble(%struct.Double*, %struct.Double*) #0 {
  %3 = alloca %struct.Double*, align 8
  %4 = alloca %struct.Double*, align 8
  store %struct.Double* %0, %struct.Double** %3, align 8
  store %struct.Double* %1, %struct.Double** %4, align 8
  %5 = load %struct.Double*, %struct.Double** %3, align 8
  %6 = getelementptr inbounds %struct.Double, %struct.Double* %5, i32 0, i32 0
  %7 = load double, double* %6, align 8
  %8 = load %struct.Double*, %struct.Double** %4, align 8
  %9 = getelementptr inbounds %struct.Double, %struct.Double* %8, i32 0, i32 0
  %10 = load double, double* %9, align 8
  %11 = fcmp olt double %7, %10
  br i1 %11, label %12, label %14

; <label>:12:                                     ; preds = %2
  %13 = call %struct.Bool* (...) @True()
  br label %16

; <label>:14:                                     ; preds = %2
  %15 = call %struct.Bool* (...) @False()
  br label %16

; <label>:16:                                     ; preds = %14, %12
  %17 = phi %struct.Bool* [ %13, %12 ], [ %15, %14 ]
  ret %struct.Bool* %17
}

; Function Attrs: noinline nounwind optnone ssp uwtable
define %struct.Bool* @gtDouble(%struct.Double*, %struct.Double*) #0 {
  %3 = alloca %struct.Double*, align 8
  %4 = alloca %struct.Double*, align 8
  store %struct.Double* %0, %struct.Double** %3, align 8
  store %struct.Double* %1, %struct.Double** %4, align 8
  %5 = load %struct.Double*, %struct.Double** %3, align 8
  %6 = getelementptr inbounds %struct.Double, %struct.Double* %5, i32 0, i32 0
  %7 = load double, double* %6, align 8
  %8 = load %struct.Double*, %struct.Double** %4, align 8
  %9 = getelementptr inbounds %struct.Double, %struct.Double* %8, i32 0, i32 0
  %10 = load double, double* %9, align 8
  %11 = fcmp ogt double %7, %10
  br i1 %11, label %12, label %14

; <label>:12:                                     ; preds = %2
  %13 = call %struct.Bool* (...) @True()
  br label %16

; <label>:14:                                     ; preds = %2
  %15 = call %struct.Bool* (...) @False()
  br label %16

; <label>:16:                                     ; preds = %14, %12
  %17 = phi %struct.Bool* [ %13, %12 ], [ %15, %14 ]
  ret %struct.Bool* %17
}

; Function Attrs: noinline nounwind optnone ssp uwtable
define %struct.Bool* @eqDouble(%struct.Double*, %struct.Double*) #0 {
  %3 = alloca %struct.Double*, align 8
  %4 = alloca %struct.Double*, align 8
  store %struct.Double* %0, %struct.Double** %3, align 8
  store %struct.Double* %1, %struct.Double** %4, align 8
  %5 = load %struct.Double*, %struct.Double** %3, align 8
  %6 = getelementptr inbounds %struct.Double, %struct.Double* %5, i32 0, i32 0
  %7 = load double, double* %6, align 8
  %8 = load %struct.Double*, %struct.Double** %4, align 8
  %9 = getelementptr inbounds %struct.Double, %struct.Double* %8, i32 0, i32 0
  %10 = load double, double* %9, align 8
  %11 = fcmp oeq double %7, %10
  br i1 %11, label %12, label %14

; <label>:12:                                     ; preds = %2
  %13 = call %struct.Bool* (...) @True()
  br label %16

; <label>:14:                                     ; preds = %2
  %15 = call %struct.Bool* (...) @False()
  br label %16

; <label>:16:                                     ; preds = %14, %12
  %17 = phi %struct.Bool* [ %13, %12 ], [ %15, %14 ]
  ret %struct.Bool* %17
}

; Function Attrs: noinline nounwind optnone ssp uwtable
define %struct.Bool* @leqDouble(%struct.Double*, %struct.Double*) #0 {
  %3 = alloca %struct.Double*, align 8
  %4 = alloca %struct.Double*, align 8
  store %struct.Double* %0, %struct.Double** %3, align 8
  store %struct.Double* %1, %struct.Double** %4, align 8
  %5 = load %struct.Double*, %struct.Double** %3, align 8
  %6 = getelementptr inbounds %struct.Double, %struct.Double* %5, i32 0, i32 0
  %7 = load double, double* %6, align 8
  %8 = load %struct.Double*, %struct.Double** %4, align 8
  %9 = getelementptr inbounds %struct.Double, %struct.Double* %8, i32 0, i32 0
  %10 = load double, double* %9, align 8
  %11 = fcmp ole double %7, %10
  br i1 %11, label %12, label %14

; <label>:12:                                     ; preds = %2
  %13 = call %struct.Bool* (...) @True()
  br label %16

; <label>:14:                                     ; preds = %2
  %15 = call %struct.Bool* (...) @False()
  br label %16

; <label>:16:                                     ; preds = %14, %12
  %17 = phi %struct.Bool* [ %13, %12 ], [ %15, %14 ]
  ret %struct.Bool* %17
}

; Function Attrs: noinline nounwind optnone ssp uwtable
define %struct.Bool* @geqDouble(%struct.Double*, %struct.Double*) #0 {
  %3 = alloca %struct.Double*, align 8
  %4 = alloca %struct.Double*, align 8
  store %struct.Double* %0, %struct.Double** %3, align 8
  store %struct.Double* %1, %struct.Double** %4, align 8
  %5 = load %struct.Double*, %struct.Double** %3, align 8
  %6 = getelementptr inbounds %struct.Double, %struct.Double* %5, i32 0, i32 0
  %7 = load double, double* %6, align 8
  %8 = load %struct.Double*, %struct.Double** %4, align 8
  %9 = getelementptr inbounds %struct.Double, %struct.Double* %8, i32 0, i32 0
  %10 = load double, double* %9, align 8
  %11 = fcmp oge double %7, %10
  br i1 %11, label %12, label %14

; <label>:12:                                     ; preds = %2
  %13 = call %struct.Bool* (...) @True()
  br label %16

; <label>:14:                                     ; preds = %2
  %15 = call %struct.Bool* (...) @False()
  br label %16

; <label>:16:                                     ; preds = %14, %12
  %17 = phi %struct.Bool* [ %13, %12 ], [ %15, %14 ]
  ret %struct.Bool* %17
}

; Function Attrs: noinline nounwind optnone ssp uwtable
define %struct.String* @omgDebug(%struct.String*) #0 {
  %2 = alloca %struct.String*, align 8
  store %struct.String* %0, %struct.String** %2, align 8
  %3 = load %struct.String*, %struct.String** %2, align 8
  %4 = getelementptr inbounds %struct.String, %struct.String* %3, i32 0, i32 1
  %5 = load i8*, i8** %4, align 8
  %6 = call i32 (i8*, ...) @printf(i8* getelementptr inbounds ([4 x i8], [4 x i8]* @.str.3, i32 0, i32 0), i8* %5)
  %7 = load %struct.String*, %struct.String** %2, align 8
  ret %struct.String* %7
}

declare i32 @printf(i8*, ...) #2

; Function Attrs: noinline nounwind optnone ssp uwtable
define i32 @main() #0 {
  %1 = alloca i32, align 4
  %2 = alloca %struct.String*, align 8
  store i32 0, i32* %1, align 4
  %3 = call %struct.String* (...) @module_main()
  store %struct.String* %3, %struct.String** %2, align 8
  %4 = load %struct.String*, %struct.String** %2, align 8
  %5 = getelementptr inbounds %struct.String, %struct.String* %4, i32 0, i32 1
  %6 = load i8*, i8** %5, align 8
  %7 = call i32 (i8*, ...) @printf(i8* getelementptr inbounds ([4 x i8], [4 x i8]* @.str.3, i32 0, i32 0), i8* %6)
  ret i32 0
}

declare %struct.String* @module_main(...) #2

attributes #0 = { noinline nounwind optnone ssp uwtable "correctly-rounded-divide-sqrt-fp-math"="false" "disable-tail-calls"="false" "less-precise-fpmad"="false" "no-frame-pointer-elim"="true" "no-frame-pointer-elim-non-leaf" "no-infs-fp-math"="false" "no-jump-tables"="false" "no-nans-fp-math"="false" "no-signed-zeros-fp-math"="false" "no-trapping-math"="false" "stack-protector-buffer-size"="8" "target-cpu"="penryn" "target-features"="+cx16,+fxsr,+mmx,+sse,+sse2,+sse3,+sse4.1,+ssse3,+x87" "unsafe-fp-math"="false" "use-soft-float"="false" }
attributes #1 = { allocsize(0) "correctly-rounded-divide-sqrt-fp-math"="false" "disable-tail-calls"="false" "less-precise-fpmad"="false" "no-frame-pointer-elim"="true" "no-frame-pointer-elim-non-leaf" "no-infs-fp-math"="false" "no-nans-fp-math"="false" "no-signed-zeros-fp-math"="false" "no-trapping-math"="false" "stack-protector-buffer-size"="8" "target-cpu"="penryn" "target-features"="+cx16,+fxsr,+mmx,+sse,+sse2,+sse3,+sse4.1,+ssse3,+x87" "unsafe-fp-math"="false" "use-soft-float"="false" }
attributes #2 = { "correctly-rounded-divide-sqrt-fp-math"="false" "disable-tail-calls"="false" "less-precise-fpmad"="false" "no-frame-pointer-elim"="true" "no-frame-pointer-elim-non-leaf" "no-infs-fp-math"="false" "no-nans-fp-math"="false" "no-signed-zeros-fp-math"="false" "no-trapping-math"="false" "stack-protector-buffer-size"="8" "target-cpu"="penryn" "target-features"="+cx16,+fxsr,+mmx,+sse,+sse2,+sse3,+sse4.1,+ssse3,+x87" "unsafe-fp-math"="false" "use-soft-float"="false" }
attributes #3 = { nounwind readnone speculatable }
attributes #4 = { allocsize(0) }

!llvm.module.flags = !{!0, !1}
!llvm.ident = !{!2}

!0 = !{i32 1, !"wchar_size", i32 4}
!1 = !{i32 7, !"PIC Level", i32 2}
!2 = !{!"Apple LLVM version 9.1.0 (clang-902.0.39.2)"}
