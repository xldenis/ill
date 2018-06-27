; ModuleID = 'rts.c'
source_filename = "rts.c"
target datalayout = "e-m:o-i64:64-f80:128-n8:16:32:64-S128"
target triple = "x86_64-apple-macosx10.13.0"

%struct.String = type { i64, i8* }
%struct.Int = type { i64 }
%struct.Bool = type { i64 }
%struct.Double = type { double }

@.str = private unnamed_addr constant [5 x i8] c"%s%s\00", align 1
@.str.1 = private unnamed_addr constant [3 x i8] c"%s\00", align 1
@.str.2 = private unnamed_addr constant [5 x i8] c"%lld\00", align 1
@.str.3 = private unnamed_addr constant [3 x i8] c"%f\00", align 1

; Function Attrs: nounwind ssp uwtable
define noalias %struct.String* @mkString(i64) local_unnamed_addr #0 {
  %2 = tail call i8* @malloc(i64 16) #6
  %3 = bitcast i8* %2 to %struct.String*
  %4 = add i64 %0, 1
  %5 = tail call i8* @malloc(i64 %4) #6
  %6 = bitcast i8* %2 to i64*
  store i64 %0, i64* %6, align 8, !tbaa !3
  %7 = getelementptr inbounds i8, i8* %2, i64 8
  %8 = bitcast i8* %7 to i8**
  store i8* %5, i8** %8, align 8, !tbaa !9
  ret %struct.String* %3
}

; Function Attrs: nounwind allocsize(0)
declare noalias i8* @malloc(i64) local_unnamed_addr #1

; Function Attrs: nounwind ssp uwtable
define noalias %struct.String* @plusStr(%struct.String* nocapture readonly, %struct.String* nocapture readonly) local_unnamed_addr #0 {
  %3 = getelementptr inbounds %struct.String, %struct.String* %0, i64 0, i32 0
  %4 = load i64, i64* %3, align 8, !tbaa !3
  %5 = getelementptr inbounds %struct.String, %struct.String* %1, i64 0, i32 0
  %6 = load i64, i64* %5, align 8, !tbaa !3
  %7 = add i64 %6, %4
  %8 = shl i64 %7, 32
  %9 = ashr exact i64 %8, 32
  %10 = tail call i8* @malloc(i64 16) #7
  %11 = bitcast i8* %10 to %struct.String*
  %12 = add nsw i64 %9, 1
  %13 = tail call i8* @malloc(i64 %12) #7
  %14 = bitcast i8* %10 to i64*
  store i64 %9, i64* %14, align 8, !tbaa !3
  %15 = getelementptr inbounds i8, i8* %10, i64 8
  %16 = bitcast i8* %15 to i8**
  store i8* %13, i8** %16, align 8, !tbaa !9
  %17 = add i64 %8, 4294967296
  %18 = ashr exact i64 %17, 32
  %19 = tail call i64 @llvm.objectsize.i64.p0i8(i8* %13, i1 false, i1 true)
  %20 = getelementptr inbounds %struct.String, %struct.String* %0, i64 0, i32 1
  %21 = load i8*, i8** %20, align 8, !tbaa !9
  %22 = getelementptr inbounds %struct.String, %struct.String* %1, i64 0, i32 1
  %23 = load i8*, i8** %22, align 8, !tbaa !9
  %24 = tail call i32 (i8*, i64, i32, i64, i8*, ...) @__snprintf_chk(i8* %13, i64 %18, i32 0, i64 %19, i8* getelementptr inbounds ([5 x i8], [5 x i8]* @.str, i64 0, i64 0), i8* %21, i8* %23) #5
  ret %struct.String* %11
}

declare i32 @__snprintf_chk(i8*, i64, i32, i64, i8*, ...) local_unnamed_addr #2

; Function Attrs: nounwind readnone speculatable
declare i64 @llvm.objectsize.i64.p0i8(i8*, i1, i1) #3

; Function Attrs: nounwind ssp uwtable
define noalias %struct.String* @cloneStr(%struct.String* nocapture readonly, %struct.Int* nocapture readonly, %struct.Int* nocapture readonly) local_unnamed_addr #0 {
  %4 = getelementptr inbounds %struct.Int, %struct.Int* %2, i64 0, i32 0
  %5 = load i64, i64* %4, align 8, !tbaa !10
  %6 = getelementptr inbounds %struct.Int, %struct.Int* %1, i64 0, i32 0
  %7 = load i64, i64* %6, align 8, !tbaa !10
  %8 = sub i64 %5, %7
  %9 = shl i64 %8, 32
  %10 = ashr exact i64 %9, 32
  %11 = tail call i8* @malloc(i64 16) #7
  %12 = bitcast i8* %11 to %struct.String*
  %13 = add nsw i64 %10, 1
  %14 = tail call i8* @malloc(i64 %13) #7
  %15 = bitcast i8* %11 to i64*
  store i64 %10, i64* %15, align 8, !tbaa !3
  %16 = getelementptr inbounds i8, i8* %11, i64 8
  %17 = bitcast i8* %16 to i8**
  store i8* %14, i8** %17, align 8, !tbaa !9
  %18 = add i64 %9, 4294967296
  %19 = ashr exact i64 %18, 32
  %20 = tail call i64 @llvm.objectsize.i64.p0i8(i8* %14, i1 false, i1 true)
  %21 = getelementptr inbounds %struct.String, %struct.String* %0, i64 0, i32 1
  %22 = load i8*, i8** %21, align 8, !tbaa !9
  %23 = getelementptr inbounds i8, i8* %22, i64 %7
  %24 = tail call i32 (i8*, i64, i32, i64, i8*, ...) @__snprintf_chk(i8* %14, i64 %19, i32 0, i64 %20, i8* getelementptr inbounds ([3 x i8], [3 x i8]* @.str.1, i64 0, i64 0), i8* %23) #5
  ret %struct.String* %12
}

; Function Attrs: nounwind ssp uwtable
define noalias %struct.String* @showInt(%struct.Int* nocapture readonly) local_unnamed_addr #0 {
  %2 = tail call i64 @llvm.objectsize.i64.p0i8(i8* null, i1 false, i1 true)
  %3 = getelementptr inbounds %struct.Int, %struct.Int* %0, i64 0, i32 0
  %4 = load i64, i64* %3, align 8, !tbaa !10
  %5 = tail call i32 (i8*, i64, i32, i64, i8*, ...) @__snprintf_chk(i8* null, i64 0, i32 0, i64 %2, i8* getelementptr inbounds ([5 x i8], [5 x i8]* @.str.2, i64 0, i64 0), i64 %4) #5
  %6 = add nsw i32 %5, 1
  %7 = sext i32 %6 to i64
  %8 = tail call i8* @malloc(i64 16) #7
  %9 = bitcast i8* %8 to %struct.String*
  %10 = add nsw i64 %7, 1
  %11 = tail call i8* @malloc(i64 %10) #7
  %12 = bitcast i8* %8 to i64*
  store i64 %7, i64* %12, align 8, !tbaa !3
  %13 = getelementptr inbounds i8, i8* %8, i64 8
  %14 = bitcast i8* %13 to i8**
  store i8* %11, i8** %14, align 8, !tbaa !9
  %15 = tail call i64 @llvm.objectsize.i64.p0i8(i8* %11, i1 false, i1 true)
  %16 = load i64, i64* %3, align 8, !tbaa !10
  %17 = tail call i32 (i8*, i64, i32, i64, i8*, ...) @__snprintf_chk(i8* %11, i64 %7, i32 0, i64 %15, i8* getelementptr inbounds ([5 x i8], [5 x i8]* @.str.2, i64 0, i64 0), i64 %16) #5
  ret %struct.String* %9
}

; Function Attrs: nounwind ssp uwtable
define %struct.Int* @plusInt(%struct.Int* nocapture readonly, %struct.Int* nocapture readonly) local_unnamed_addr #0 {
  %3 = getelementptr inbounds %struct.Int, %struct.Int* %0, i64 0, i32 0
  %4 = load i64, i64* %3, align 8, !tbaa !10
  %5 = getelementptr inbounds %struct.Int, %struct.Int* %1, i64 0, i32 0
  %6 = load i64, i64* %5, align 8, !tbaa !10
  %7 = add i64 %6, %4
  %8 = tail call %struct.Int* @mkInt(i64 %7) #5
  ret %struct.Int* %8
}

declare %struct.Int* @mkInt(i64) local_unnamed_addr #2

; Function Attrs: nounwind ssp uwtable
define %struct.Int* @minusInt(%struct.Int* nocapture readonly, %struct.Int* nocapture readonly) local_unnamed_addr #0 {
  %3 = getelementptr inbounds %struct.Int, %struct.Int* %0, i64 0, i32 0
  %4 = load i64, i64* %3, align 8, !tbaa !10
  %5 = getelementptr inbounds %struct.Int, %struct.Int* %1, i64 0, i32 0
  %6 = load i64, i64* %5, align 8, !tbaa !10
  %7 = sub i64 %4, %6
  %8 = tail call %struct.Int* @mkInt(i64 %7) #5
  ret %struct.Int* %8
}

; Function Attrs: nounwind ssp uwtable
define %struct.Int* @multInt(%struct.Int* nocapture readonly, %struct.Int* nocapture readonly) local_unnamed_addr #0 {
  %3 = getelementptr inbounds %struct.Int, %struct.Int* %0, i64 0, i32 0
  %4 = load i64, i64* %3, align 8, !tbaa !10
  %5 = getelementptr inbounds %struct.Int, %struct.Int* %1, i64 0, i32 0
  %6 = load i64, i64* %5, align 8, !tbaa !10
  %7 = mul i64 %6, %4
  %8 = tail call %struct.Int* @mkInt(i64 %7) #5
  ret %struct.Int* %8
}

; Function Attrs: nounwind ssp uwtable
define %struct.Int* @divInt(%struct.Int* nocapture readonly, %struct.Int* nocapture readonly) local_unnamed_addr #0 {
  %3 = getelementptr inbounds %struct.Int, %struct.Int* %0, i64 0, i32 0
  %4 = load i64, i64* %3, align 8, !tbaa !10
  %5 = getelementptr inbounds %struct.Int, %struct.Int* %1, i64 0, i32 0
  %6 = load i64, i64* %5, align 8, !tbaa !10
  %7 = udiv i64 %4, %6
  %8 = tail call %struct.Int* @mkInt(i64 %7) #5
  ret %struct.Int* %8
}

; Function Attrs: nounwind ssp uwtable
define %struct.Int* @modInt(%struct.Int* nocapture readonly, %struct.Int* nocapture readonly) local_unnamed_addr #0 {
  %3 = getelementptr inbounds %struct.Int, %struct.Int* %0, i64 0, i32 0
  %4 = load i64, i64* %3, align 8, !tbaa !10
  %5 = getelementptr inbounds %struct.Int, %struct.Int* %1, i64 0, i32 0
  %6 = load i64, i64* %5, align 8, !tbaa !10
  %7 = urem i64 %4, %6
  %8 = tail call %struct.Int* @mkInt(i64 %7) #5
  ret %struct.Int* %8
}

; Function Attrs: nounwind ssp uwtable
define %struct.Bool* @ltInt(%struct.Int* nocapture readonly, %struct.Int* nocapture readonly) local_unnamed_addr #0 {
  %3 = getelementptr inbounds %struct.Int, %struct.Int* %0, i64 0, i32 0
  %4 = load i64, i64* %3, align 8, !tbaa !10
  %5 = getelementptr inbounds %struct.Int, %struct.Int* %1, i64 0, i32 0
  %6 = load i64, i64* %5, align 8, !tbaa !10
  %7 = icmp ult i64 %4, %6
  br i1 %7, label %8, label %10

; <label>:8:                                      ; preds = %2
  %9 = tail call %struct.Bool* (...) @True() #5
  br label %12

; <label>:10:                                     ; preds = %2
  %11 = tail call %struct.Bool* (...) @False() #5
  br label %12

; <label>:12:                                     ; preds = %10, %8
  %13 = phi %struct.Bool* [ %9, %8 ], [ %11, %10 ]
  ret %struct.Bool* %13
}

declare %struct.Bool* @True(...) local_unnamed_addr #2

declare %struct.Bool* @False(...) local_unnamed_addr #2

; Function Attrs: nounwind ssp uwtable
define %struct.Bool* @gtInt(%struct.Int* nocapture readonly, %struct.Int* nocapture readonly) local_unnamed_addr #0 {
  %3 = getelementptr inbounds %struct.Int, %struct.Int* %0, i64 0, i32 0
  %4 = load i64, i64* %3, align 8, !tbaa !10
  %5 = getelementptr inbounds %struct.Int, %struct.Int* %1, i64 0, i32 0
  %6 = load i64, i64* %5, align 8, !tbaa !10
  %7 = icmp ugt i64 %4, %6
  br i1 %7, label %8, label %10

; <label>:8:                                      ; preds = %2
  %9 = tail call %struct.Bool* (...) @True() #5
  br label %12

; <label>:10:                                     ; preds = %2
  %11 = tail call %struct.Bool* (...) @False() #5
  br label %12

; <label>:12:                                     ; preds = %10, %8
  %13 = phi %struct.Bool* [ %9, %8 ], [ %11, %10 ]
  ret %struct.Bool* %13
}

; Function Attrs: nounwind ssp uwtable
define %struct.Bool* @eqInt(%struct.Int* nocapture readonly, %struct.Int* nocapture readonly) local_unnamed_addr #0 {
  %3 = getelementptr inbounds %struct.Int, %struct.Int* %0, i64 0, i32 0
  %4 = load i64, i64* %3, align 8, !tbaa !10
  %5 = getelementptr inbounds %struct.Int, %struct.Int* %1, i64 0, i32 0
  %6 = load i64, i64* %5, align 8, !tbaa !10
  %7 = icmp eq i64 %4, %6
  br i1 %7, label %8, label %10

; <label>:8:                                      ; preds = %2
  %9 = tail call %struct.Bool* (...) @True() #5
  br label %12

; <label>:10:                                     ; preds = %2
  %11 = tail call %struct.Bool* (...) @False() #5
  br label %12

; <label>:12:                                     ; preds = %10, %8
  %13 = phi %struct.Bool* [ %9, %8 ], [ %11, %10 ]
  ret %struct.Bool* %13
}

; Function Attrs: nounwind ssp uwtable
define %struct.Bool* @leqInt(%struct.Int* nocapture readonly, %struct.Int* nocapture readonly) local_unnamed_addr #0 {
  %3 = getelementptr inbounds %struct.Int, %struct.Int* %0, i64 0, i32 0
  %4 = load i64, i64* %3, align 8, !tbaa !10
  %5 = getelementptr inbounds %struct.Int, %struct.Int* %1, i64 0, i32 0
  %6 = load i64, i64* %5, align 8, !tbaa !10
  %7 = icmp ugt i64 %4, %6
  br i1 %7, label %10, label %8

; <label>:8:                                      ; preds = %2
  %9 = tail call %struct.Bool* (...) @True() #5
  br label %12

; <label>:10:                                     ; preds = %2
  %11 = tail call %struct.Bool* (...) @False() #5
  br label %12

; <label>:12:                                     ; preds = %10, %8
  %13 = phi %struct.Bool* [ %9, %8 ], [ %11, %10 ]
  ret %struct.Bool* %13
}

; Function Attrs: nounwind ssp uwtable
define %struct.Bool* @geqInt(%struct.Int* nocapture readonly, %struct.Int* nocapture readonly) local_unnamed_addr #0 {
  %3 = getelementptr inbounds %struct.Int, %struct.Int* %0, i64 0, i32 0
  %4 = load i64, i64* %3, align 8, !tbaa !10
  %5 = getelementptr inbounds %struct.Int, %struct.Int* %1, i64 0, i32 0
  %6 = load i64, i64* %5, align 8, !tbaa !10
  %7 = icmp ult i64 %4, %6
  br i1 %7, label %10, label %8

; <label>:8:                                      ; preds = %2
  %9 = tail call %struct.Bool* (...) @True() #5
  br label %12

; <label>:10:                                     ; preds = %2
  %11 = tail call %struct.Bool* (...) @False() #5
  br label %12

; <label>:12:                                     ; preds = %10, %8
  %13 = phi %struct.Bool* [ %9, %8 ], [ %11, %10 ]
  ret %struct.Bool* %13
}

; Function Attrs: nounwind ssp uwtable
define noalias %struct.String* @showDouble(%struct.Double* nocapture readonly) local_unnamed_addr #0 {
  %2 = tail call i64 @llvm.objectsize.i64.p0i8(i8* null, i1 false, i1 true)
  %3 = getelementptr inbounds %struct.Double, %struct.Double* %0, i64 0, i32 0
  %4 = load double, double* %3, align 8, !tbaa !13
  %5 = tail call i32 (i8*, i64, i32, i64, i8*, ...) @__snprintf_chk(i8* null, i64 0, i32 0, i64 %2, i8* getelementptr inbounds ([3 x i8], [3 x i8]* @.str.3, i64 0, i64 0), double %4) #5
  %6 = add nsw i32 %5, 1
  %7 = sext i32 %6 to i64
  %8 = tail call i8* @malloc(i64 16) #7
  %9 = bitcast i8* %8 to %struct.String*
  %10 = add nsw i64 %7, 1
  %11 = tail call i8* @malloc(i64 %10) #7
  %12 = bitcast i8* %8 to i64*
  store i64 %7, i64* %12, align 8, !tbaa !3
  %13 = getelementptr inbounds i8, i8* %8, i64 8
  %14 = bitcast i8* %13 to i8**
  store i8* %11, i8** %14, align 8, !tbaa !9
  %15 = tail call i64 @llvm.objectsize.i64.p0i8(i8* %11, i1 false, i1 true)
  %16 = load double, double* %3, align 8, !tbaa !13
  %17 = tail call i32 (i8*, i64, i32, i64, i8*, ...) @__snprintf_chk(i8* %11, i64 %7, i32 0, i64 %15, i8* getelementptr inbounds ([3 x i8], [3 x i8]* @.str.3, i64 0, i64 0), double %16) #5
  ret %struct.String* %9
}

; Function Attrs: nounwind ssp uwtable
define %struct.Double* @plusDouble(%struct.Double* nocapture readonly, %struct.Double* nocapture readonly) local_unnamed_addr #0 {
  %3 = getelementptr inbounds %struct.Double, %struct.Double* %0, i64 0, i32 0
  %4 = load double, double* %3, align 8, !tbaa !13
  %5 = getelementptr inbounds %struct.Double, %struct.Double* %1, i64 0, i32 0
  %6 = load double, double* %5, align 8, !tbaa !13
  %7 = fadd double %4, %6
  %8 = tail call %struct.Double* @mkDouble(double %7) #5
  ret %struct.Double* %8
}

declare %struct.Double* @mkDouble(double) local_unnamed_addr #2

; Function Attrs: nounwind ssp uwtable
define %struct.Double* @minusDouble(%struct.Double* nocapture readonly, %struct.Double* nocapture readonly) local_unnamed_addr #0 {
  %3 = getelementptr inbounds %struct.Double, %struct.Double* %0, i64 0, i32 0
  %4 = load double, double* %3, align 8, !tbaa !13
  %5 = getelementptr inbounds %struct.Double, %struct.Double* %1, i64 0, i32 0
  %6 = load double, double* %5, align 8, !tbaa !13
  %7 = fsub double %4, %6
  %8 = tail call %struct.Double* @mkDouble(double %7) #5
  ret %struct.Double* %8
}

; Function Attrs: nounwind ssp uwtable
define %struct.Double* @multDouble(%struct.Double* nocapture readonly, %struct.Double* nocapture readonly) local_unnamed_addr #0 {
  %3 = getelementptr inbounds %struct.Double, %struct.Double* %0, i64 0, i32 0
  %4 = load double, double* %3, align 8, !tbaa !13
  %5 = getelementptr inbounds %struct.Double, %struct.Double* %1, i64 0, i32 0
  %6 = load double, double* %5, align 8, !tbaa !13
  %7 = fmul double %4, %6
  %8 = tail call %struct.Double* @mkDouble(double %7) #5
  ret %struct.Double* %8
}

; Function Attrs: nounwind ssp uwtable
define %struct.Double* @divDouble(%struct.Double* nocapture readonly, %struct.Double* nocapture readonly) local_unnamed_addr #0 {
  %3 = getelementptr inbounds %struct.Double, %struct.Double* %0, i64 0, i32 0
  %4 = load double, double* %3, align 8, !tbaa !13
  %5 = getelementptr inbounds %struct.Double, %struct.Double* %1, i64 0, i32 0
  %6 = load double, double* %5, align 8, !tbaa !13
  %7 = fdiv double %4, %6
  %8 = tail call %struct.Double* @mkDouble(double %7) #5
  ret %struct.Double* %8
}

; Function Attrs: nounwind ssp uwtable
define %struct.Double* @modDouble(%struct.Double* nocapture readonly, %struct.Double* nocapture readonly) local_unnamed_addr #0 {
  %3 = getelementptr inbounds %struct.Double, %struct.Double* %0, i64 0, i32 0
  %4 = load double, double* %3, align 8, !tbaa !13
  %5 = getelementptr inbounds %struct.Double, %struct.Double* %1, i64 0, i32 0
  %6 = load double, double* %5, align 8, !tbaa !13
  %7 = tail call double @fmod(double %4, double %6) #8
  %8 = tail call %struct.Double* @mkDouble(double %7) #5
  ret %struct.Double* %8
}

; Function Attrs: nounwind readnone
declare double @fmod(double, double) local_unnamed_addr #4

; Function Attrs: nounwind ssp uwtable
define %struct.Bool* @ltDouble(%struct.Double* nocapture readonly, %struct.Double* nocapture readonly) local_unnamed_addr #0 {
  %3 = getelementptr inbounds %struct.Double, %struct.Double* %0, i64 0, i32 0
  %4 = load double, double* %3, align 8, !tbaa !13
  %5 = getelementptr inbounds %struct.Double, %struct.Double* %1, i64 0, i32 0
  %6 = load double, double* %5, align 8, !tbaa !13
  %7 = fcmp olt double %4, %6
  br i1 %7, label %8, label %10

; <label>:8:                                      ; preds = %2
  %9 = tail call %struct.Bool* (...) @True() #5
  br label %12

; <label>:10:                                     ; preds = %2
  %11 = tail call %struct.Bool* (...) @False() #5
  br label %12

; <label>:12:                                     ; preds = %10, %8
  %13 = phi %struct.Bool* [ %9, %8 ], [ %11, %10 ]
  ret %struct.Bool* %13
}

; Function Attrs: nounwind ssp uwtable
define %struct.Bool* @gtDouble(%struct.Double* nocapture readonly, %struct.Double* nocapture readonly) local_unnamed_addr #0 {
  %3 = getelementptr inbounds %struct.Double, %struct.Double* %0, i64 0, i32 0
  %4 = load double, double* %3, align 8, !tbaa !13
  %5 = getelementptr inbounds %struct.Double, %struct.Double* %1, i64 0, i32 0
  %6 = load double, double* %5, align 8, !tbaa !13
  %7 = fcmp ogt double %4, %6
  br i1 %7, label %8, label %10

; <label>:8:                                      ; preds = %2
  %9 = tail call %struct.Bool* (...) @True() #5
  br label %12

; <label>:10:                                     ; preds = %2
  %11 = tail call %struct.Bool* (...) @False() #5
  br label %12

; <label>:12:                                     ; preds = %10, %8
  %13 = phi %struct.Bool* [ %9, %8 ], [ %11, %10 ]
  ret %struct.Bool* %13
}

; Function Attrs: nounwind ssp uwtable
define %struct.Bool* @eqDouble(%struct.Double* nocapture readonly, %struct.Double* nocapture readonly) local_unnamed_addr #0 {
  %3 = getelementptr inbounds %struct.Double, %struct.Double* %0, i64 0, i32 0
  %4 = load double, double* %3, align 8, !tbaa !13
  %5 = getelementptr inbounds %struct.Double, %struct.Double* %1, i64 0, i32 0
  %6 = load double, double* %5, align 8, !tbaa !13
  %7 = fcmp oeq double %4, %6
  br i1 %7, label %8, label %10

; <label>:8:                                      ; preds = %2
  %9 = tail call %struct.Bool* (...) @True() #5
  br label %12

; <label>:10:                                     ; preds = %2
  %11 = tail call %struct.Bool* (...) @False() #5
  br label %12

; <label>:12:                                     ; preds = %10, %8
  %13 = phi %struct.Bool* [ %9, %8 ], [ %11, %10 ]
  ret %struct.Bool* %13
}

; Function Attrs: nounwind ssp uwtable
define %struct.Bool* @leqDouble(%struct.Double* nocapture readonly, %struct.Double* nocapture readonly) local_unnamed_addr #0 {
  %3 = getelementptr inbounds %struct.Double, %struct.Double* %0, i64 0, i32 0
  %4 = load double, double* %3, align 8, !tbaa !13
  %5 = getelementptr inbounds %struct.Double, %struct.Double* %1, i64 0, i32 0
  %6 = load double, double* %5, align 8, !tbaa !13
  %7 = fcmp ugt double %4, %6
  br i1 %7, label %10, label %8

; <label>:8:                                      ; preds = %2
  %9 = tail call %struct.Bool* (...) @True() #5
  br label %12

; <label>:10:                                     ; preds = %2
  %11 = tail call %struct.Bool* (...) @False() #5
  br label %12

; <label>:12:                                     ; preds = %10, %8
  %13 = phi %struct.Bool* [ %9, %8 ], [ %11, %10 ]
  ret %struct.Bool* %13
}

; Function Attrs: nounwind ssp uwtable
define %struct.Bool* @geqDouble(%struct.Double* nocapture readonly, %struct.Double* nocapture readonly) local_unnamed_addr #0 {
  %3 = getelementptr inbounds %struct.Double, %struct.Double* %0, i64 0, i32 0
  %4 = load double, double* %3, align 8, !tbaa !13
  %5 = getelementptr inbounds %struct.Double, %struct.Double* %1, i64 0, i32 0
  %6 = load double, double* %5, align 8, !tbaa !13
  %7 = fcmp ult double %4, %6
  br i1 %7, label %10, label %8

; <label>:8:                                      ; preds = %2
  %9 = tail call %struct.Bool* (...) @True() #5
  br label %12

; <label>:10:                                     ; preds = %2
  %11 = tail call %struct.Bool* (...) @False() #5
  br label %12

; <label>:12:                                     ; preds = %10, %8
  %13 = phi %struct.Bool* [ %9, %8 ], [ %11, %10 ]
  ret %struct.Bool* %13
}

; Function Attrs: nounwind ssp uwtable
define %struct.String* @omgDebug(%struct.String* readonly returned) local_unnamed_addr #0 {
  %2 = getelementptr inbounds %struct.String, %struct.String* %0, i64 0, i32 1
  %3 = load i8*, i8** %2, align 8, !tbaa !9
  %4 = tail call i32 @puts(i8* %3)
  ret %struct.String* %0
}

; Function Attrs: nounwind ssp uwtable
define i32 @main() local_unnamed_addr #0 {
  %1 = tail call %struct.String* (...) @module_main() #5
  %2 = getelementptr inbounds %struct.String, %struct.String* %1, i64 0, i32 1
  %3 = load i8*, i8** %2, align 8, !tbaa !9
  %4 = tail call i32 @puts(i8* %3)
  ret i32 0
}

declare %struct.String* @module_main(...) local_unnamed_addr #2

; Function Attrs: nounwind
declare i32 @puts(i8* nocapture readonly) #5

attributes #0 = { nounwind ssp uwtable "correctly-rounded-divide-sqrt-fp-math"="false" "disable-tail-calls"="false" "less-precise-fpmad"="false" "no-frame-pointer-elim"="true" "no-frame-pointer-elim-non-leaf" "no-infs-fp-math"="false" "no-jump-tables"="false" "no-nans-fp-math"="false" "no-signed-zeros-fp-math"="false" "no-trapping-math"="false" "stack-protector-buffer-size"="8" "target-cpu"="penryn" "target-features"="+cx16,+fxsr,+mmx,+sse,+sse2,+sse3,+sse4.1,+ssse3,+x87" "unsafe-fp-math"="false" "use-soft-float"="false" }
attributes #1 = { nounwind allocsize(0) "correctly-rounded-divide-sqrt-fp-math"="false" "disable-tail-calls"="false" "less-precise-fpmad"="false" "no-frame-pointer-elim"="true" "no-frame-pointer-elim-non-leaf" "no-infs-fp-math"="false" "no-nans-fp-math"="false" "no-signed-zeros-fp-math"="false" "no-trapping-math"="false" "stack-protector-buffer-size"="8" "target-cpu"="penryn" "target-features"="+cx16,+fxsr,+mmx,+sse,+sse2,+sse3,+sse4.1,+ssse3,+x87" "unsafe-fp-math"="false" "use-soft-float"="false" }
attributes #2 = { "correctly-rounded-divide-sqrt-fp-math"="false" "disable-tail-calls"="false" "less-precise-fpmad"="false" "no-frame-pointer-elim"="true" "no-frame-pointer-elim-non-leaf" "no-infs-fp-math"="false" "no-nans-fp-math"="false" "no-signed-zeros-fp-math"="false" "no-trapping-math"="false" "stack-protector-buffer-size"="8" "target-cpu"="penryn" "target-features"="+cx16,+fxsr,+mmx,+sse,+sse2,+sse3,+sse4.1,+ssse3,+x87" "unsafe-fp-math"="false" "use-soft-float"="false" }
attributes #3 = { nounwind readnone speculatable }
attributes #4 = { nounwind readnone "correctly-rounded-divide-sqrt-fp-math"="false" "disable-tail-calls"="false" "less-precise-fpmad"="false" "no-frame-pointer-elim"="true" "no-frame-pointer-elim-non-leaf" "no-infs-fp-math"="false" "no-nans-fp-math"="false" "no-signed-zeros-fp-math"="false" "no-trapping-math"="false" "stack-protector-buffer-size"="8" "target-cpu"="penryn" "target-features"="+cx16,+fxsr,+mmx,+sse,+sse2,+sse3,+sse4.1,+ssse3,+x87" "unsafe-fp-math"="false" "use-soft-float"="false" }
attributes #5 = { nounwind }
attributes #6 = { allocsize(0) }
attributes #7 = { nounwind allocsize(0) }
attributes #8 = { nounwind readnone }

!llvm.module.flags = !{!0, !1}
!llvm.ident = !{!2}

!0 = !{i32 1, !"wchar_size", i32 4}
!1 = !{i32 7, !"PIC Level", i32 2}
!2 = !{!"Apple LLVM version 9.1.0 (clang-902.0.39.2)"}
!3 = !{!4, !5, i64 0}
!4 = !{!"", !5, i64 0, !8, i64 8}
!5 = !{!"long", !6, i64 0}
!6 = !{!"omnipotent char", !7, i64 0}
!7 = !{!"Simple C/C++ TBAA"}
!8 = !{!"any pointer", !6, i64 0}
!9 = !{!4, !8, i64 8}
!10 = !{!11, !12, i64 0}
!11 = !{!"", !12, i64 0}
!12 = !{!"long long", !6, i64 0}
!13 = !{!14, !15, i64 0}
!14 = !{!"", !15, i64 0}
!15 = !{!"double", !6, i64 0}
