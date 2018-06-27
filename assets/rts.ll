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

; Function Attrs: norecurse nounwind readonly ssp uwtable
define i8* @utf8index(i8* readonly, i64) local_unnamed_addr #0 {
  %3 = load i8, i8* %0, align 1, !tbaa !3
  %4 = icmp eq i8 %3, 0
  br i1 %4, label %20, label %5

; <label>:5:                                      ; preds = %2
  %6 = add i64 %1, 1
  br label %7

; <label>:7:                                      ; preds = %5, %16
  %8 = phi i8 [ %3, %5 ], [ %18, %16 ]
  %9 = phi i64 [ %6, %5 ], [ %14, %16 ]
  %10 = phi i8* [ %0, %5 ], [ %17, %16 ]
  %11 = and i8 %8, -64
  %12 = icmp ne i8 %11, -128
  %13 = sext i1 %12 to i64
  %14 = add i64 %9, %13
  %15 = icmp eq i64 %14, 0
  br i1 %15, label %20, label %16

; <label>:16:                                     ; preds = %7
  %17 = getelementptr inbounds i8, i8* %10, i64 1
  %18 = load i8, i8* %17, align 1, !tbaa !3
  %19 = icmp eq i8 %18, 0
  br i1 %19, label %20, label %7

; <label>:20:                                     ; preds = %7, %16, %2
  %21 = phi i8* [ null, %2 ], [ null, %16 ], [ %10, %7 ]
  ret i8* %21
}

; Function Attrs: norecurse nounwind readonly ssp uwtable
define i64 @utf8_codepoints_len(i8* nocapture readonly) local_unnamed_addr #0 {
  %2 = load i8, i8* %0, align 1, !tbaa !3
  %3 = icmp eq i8 %2, 0
  br i1 %3, label %16, label %4

; <label>:4:                                      ; preds = %1
  br label %5

; <label>:5:                                      ; preds = %4, %5
  %6 = phi i8 [ %14, %5 ], [ %2, %4 ]
  %7 = phi i64 [ %13, %5 ], [ 0, %4 ]
  %8 = phi i8* [ %9, %5 ], [ %0, %4 ]
  %9 = getelementptr inbounds i8, i8* %8, i64 1
  %10 = and i8 %6, -64
  %11 = icmp ne i8 %10, -128
  %12 = zext i1 %11 to i64
  %13 = add i64 %7, %12
  %14 = load i8, i8* %9, align 1, !tbaa !3
  %15 = icmp eq i8 %14, 0
  br i1 %15, label %16, label %5

; <label>:16:                                     ; preds = %5, %1
  %17 = phi i64 [ 0, %1 ], [ %13, %5 ]
  ret i64 %17
}

; Function Attrs: norecurse nounwind ssp uwtable
define void @utf8slice(i8*, i64* nocapture, i64* nocapture) local_unnamed_addr #1 {
  %4 = load i8, i8* %0, align 1, !tbaa !3
  %5 = icmp eq i8 %4, 0
  br i1 %5, label %22, label %6

; <label>:6:                                      ; preds = %3
  %7 = load i64, i64* %1, align 8, !tbaa !6
  %8 = add i64 %7, 1
  br label %9

; <label>:9:                                      ; preds = %18, %6
  %10 = phi i8 [ %4, %6 ], [ %20, %18 ]
  %11 = phi i64 [ %8, %6 ], [ %16, %18 ]
  %12 = phi i8* [ %0, %6 ], [ %19, %18 ]
  %13 = and i8 %10, -64
  %14 = icmp ne i8 %13, -128
  %15 = sext i1 %14 to i64
  %16 = add i64 %11, %15
  %17 = icmp eq i64 %16, 0
  br i1 %17, label %22, label %18

; <label>:18:                                     ; preds = %9
  %19 = getelementptr inbounds i8, i8* %12, i64 1
  %20 = load i8, i8* %19, align 1, !tbaa !3
  %21 = icmp eq i8 %20, 0
  br i1 %21, label %22, label %9

; <label>:22:                                     ; preds = %9, %18, %3
  %23 = phi i8* [ null, %3 ], [ %12, %9 ], [ null, %18 ]
  %24 = icmp eq i8* %23, null
  %25 = ptrtoint i8* %23 to i64
  %26 = ptrtoint i8* %0 to i64
  %27 = sub i64 %25, %26
  %28 = select i1 %24, i64 -1, i64 %27
  store i64 %28, i64* %1, align 8, !tbaa !6
  %29 = load i8, i8* %0, align 1, !tbaa !3
  %30 = icmp eq i8 %29, 0
  br i1 %30, label %47, label %31

; <label>:31:                                     ; preds = %22
  %32 = load i64, i64* %2, align 8, !tbaa !6
  %33 = add i64 %32, 1
  br label %34

; <label>:34:                                     ; preds = %43, %31
  %35 = phi i8 [ %29, %31 ], [ %45, %43 ]
  %36 = phi i64 [ %33, %31 ], [ %41, %43 ]
  %37 = phi i8* [ %0, %31 ], [ %44, %43 ]
  %38 = and i8 %35, -64
  %39 = icmp ne i8 %38, -128
  %40 = sext i1 %39 to i64
  %41 = add i64 %36, %40
  %42 = icmp eq i64 %41, 0
  br i1 %42, label %47, label %43

; <label>:43:                                     ; preds = %34
  %44 = getelementptr inbounds i8, i8* %37, i64 1
  %45 = load i8, i8* %44, align 1, !tbaa !3
  %46 = icmp eq i8 %45, 0
  br i1 %46, label %47, label %34

; <label>:47:                                     ; preds = %34, %43, %22
  %48 = phi i8* [ null, %22 ], [ %37, %34 ], [ null, %43 ]
  %49 = icmp eq i8* %48, null
  %50 = ptrtoint i8* %48 to i64
  %51 = sub i64 %50, %26
  %52 = select i1 %49, i64 -1, i64 %51
  store i64 %52, i64* %2, align 8, !tbaa !6
  ret void
}

; Function Attrs: nounwind ssp uwtable
define noalias %struct.String* @mkString(i64) local_unnamed_addr #2 {
  %2 = tail call i8* @malloc(i64 16) #9
  %3 = bitcast i8* %2 to %struct.String*
  %4 = add i64 %0, 1
  %5 = tail call i8* @malloc(i64 %4) #9
  %6 = bitcast i8* %2 to i64*
  store i64 0, i64* %6, align 8, !tbaa !8
  %7 = getelementptr inbounds i8, i8* %2, i64 8
  %8 = bitcast i8* %7 to i8**
  store i8* %5, i8** %8, align 8, !tbaa !11
  ret %struct.String* %3
}

; Function Attrs: nounwind allocsize(0)
declare noalias i8* @malloc(i64) local_unnamed_addr #3

; Function Attrs: nounwind ssp uwtable
define noalias %struct.String* @plusStr(%struct.String* nocapture readonly, %struct.String* nocapture readonly) local_unnamed_addr #2 {
  %3 = getelementptr inbounds %struct.String, %struct.String* %0, i64 0, i32 0
  %4 = load i64, i64* %3, align 8, !tbaa !8
  %5 = getelementptr inbounds %struct.String, %struct.String* %1, i64 0, i32 0
  %6 = load i64, i64* %5, align 8, !tbaa !8
  %7 = add i64 %6, %4
  %8 = getelementptr inbounds %struct.String, %struct.String* %0, i64 0, i32 1
  %9 = load i8*, i8** %8, align 8, !tbaa !11
  %10 = tail call i64 @strlen(i8* %9)
  %11 = getelementptr inbounds %struct.String, %struct.String* %1, i64 0, i32 1
  %12 = load i8*, i8** %11, align 8, !tbaa !11
  %13 = tail call i64 @strlen(i8* %12)
  %14 = add i64 %13, %10
  %15 = tail call i8* @malloc(i64 16) #10
  %16 = bitcast i8* %15 to %struct.String*
  %17 = add i64 %14, 1
  %18 = tail call i8* @malloc(i64 %17) #10
  %19 = bitcast i8* %15 to i64*
  %20 = getelementptr inbounds i8, i8* %15, i64 8
  %21 = bitcast i8* %20 to i8**
  store i8* %18, i8** %21, align 8, !tbaa !11
  %22 = shl i64 %7, 32
  %23 = ashr exact i64 %22, 32
  store i64 %23, i64* %19, align 8, !tbaa !8
  %24 = shl i64 %14, 32
  %25 = add i64 %24, 4294967296
  %26 = ashr exact i64 %25, 32
  %27 = tail call i64 @llvm.objectsize.i64.p0i8(i8* %18, i1 false, i1 true)
  %28 = tail call i32 (i8*, i64, i32, i64, i8*, ...) @__snprintf_chk(i8* %18, i64 %26, i32 0, i64 %27, i8* getelementptr inbounds ([5 x i8], [5 x i8]* @.str, i64 0, i64 0), i8* %9, i8* %12) #8
  ret %struct.String* %16
}

; Function Attrs: argmemonly nounwind readonly
declare i64 @strlen(i8* nocapture) local_unnamed_addr #4

declare i32 @__snprintf_chk(i8*, i64, i32, i64, i8*, ...) local_unnamed_addr #5

; Function Attrs: nounwind readnone speculatable
declare i64 @llvm.objectsize.i64.p0i8(i8*, i1, i1) #6

; Function Attrs: nounwind ssp uwtable
define noalias %struct.String* @cloneStr(%struct.String* nocapture readonly, %struct.Int* nocapture readonly, %struct.Int* nocapture readonly) local_unnamed_addr #2 {
  %4 = getelementptr inbounds %struct.Int, %struct.Int* %2, i64 0, i32 0
  %5 = load i64, i64* %4, align 8, !tbaa !12
  %6 = getelementptr inbounds %struct.String, %struct.String* %0, i64 0, i32 1
  %7 = load i8*, i8** %6, align 8, !tbaa !11
  %8 = load i8, i8* %7, align 1, !tbaa !3
  %9 = icmp eq i8 %8, 0
  br i1 %9, label %54, label %10

; <label>:10:                                     ; preds = %3
  %11 = getelementptr inbounds %struct.Int, %struct.Int* %1, i64 0, i32 0
  %12 = load i64, i64* %11, align 8, !tbaa !12
  %13 = add i64 %12, 1
  br label %14

; <label>:14:                                     ; preds = %23, %10
  %15 = phi i8 [ %8, %10 ], [ %25, %23 ]
  %16 = phi i64 [ %13, %10 ], [ %21, %23 ]
  %17 = phi i8* [ %7, %10 ], [ %24, %23 ]
  %18 = and i8 %15, -64
  %19 = icmp ne i8 %18, -128
  %20 = sext i1 %19 to i64
  %21 = add i64 %16, %20
  %22 = icmp eq i64 %21, 0
  br i1 %22, label %27, label %23

; <label>:23:                                     ; preds = %14
  %24 = getelementptr inbounds i8, i8* %17, i64 1
  %25 = load i8, i8* %24, align 1, !tbaa !3
  %26 = icmp eq i8 %25, 0
  br i1 %26, label %27, label %14

; <label>:27:                                     ; preds = %14, %23
  %28 = phi i8* [ %17, %14 ], [ null, %23 ]
  %29 = icmp eq i8* %28, null
  %30 = ptrtoint i8* %28 to i64
  %31 = ptrtoint i8* %7 to i64
  %32 = sub i64 %30, %31
  %33 = select i1 %29, i64 -1, i64 %32
  %34 = add i64 %5, 1
  br label %35

; <label>:35:                                     ; preds = %44, %27
  %36 = phi i8 [ %8, %27 ], [ %46, %44 ]
  %37 = phi i64 [ %34, %27 ], [ %42, %44 ]
  %38 = phi i8* [ %7, %27 ], [ %45, %44 ]
  %39 = and i8 %36, -64
  %40 = icmp ne i8 %39, -128
  %41 = sext i1 %40 to i64
  %42 = add i64 %37, %41
  %43 = icmp eq i64 %42, 0
  br i1 %43, label %48, label %44

; <label>:44:                                     ; preds = %35
  %45 = getelementptr inbounds i8, i8* %38, i64 1
  %46 = load i8, i8* %45, align 1, !tbaa !3
  %47 = icmp eq i8 %46, 0
  br i1 %47, label %54, label %35

; <label>:48:                                     ; preds = %35
  %49 = icmp eq i8* %38, null
  %50 = ptrtoint i8* %38 to i64
  %51 = sub i64 %50, %31
  %52 = select i1 %49, i64 -1, i64 %51
  %53 = icmp slt i64 %52, 0
  br i1 %53, label %54, label %57

; <label>:54:                                     ; preds = %44, %3, %48
  %55 = phi i64 [ %33, %48 ], [ -1, %3 ], [ %33, %44 ]
  %56 = tail call i64 @strlen(i8* %7)
  br label %57

; <label>:57:                                     ; preds = %48, %54
  %58 = phi i64 [ %55, %54 ], [ %33, %48 ]
  %59 = phi i64 [ %56, %54 ], [ %52, %48 ]
  %60 = sub nsw i64 %59, %58
  %61 = shl i64 %60, 32
  %62 = add i64 %61, 4294967296
  %63 = ashr exact i64 %62, 32
  %64 = tail call i8* @malloc(i64 16) #10
  %65 = bitcast i8* %64 to %struct.String*
  %66 = add nsw i64 %63, 1
  %67 = tail call i8* @malloc(i64 %66) #10
  %68 = bitcast i8* %64 to i64*
  store i64 0, i64* %68, align 8, !tbaa !8
  %69 = getelementptr inbounds i8, i8* %64, i64 8
  %70 = bitcast i8* %69 to i8**
  store i8* %67, i8** %70, align 8, !tbaa !11
  %71 = tail call i64 @llvm.objectsize.i64.p0i8(i8* %67, i1 false, i1 true)
  %72 = getelementptr inbounds i8, i8* %7, i64 %58
  %73 = tail call i32 (i8*, i64, i32, i64, i8*, ...) @__snprintf_chk(i8* %67, i64 %63, i32 0, i64 %71, i8* getelementptr inbounds ([3 x i8], [3 x i8]* @.str.1, i64 0, i64 0), i8* %72) #8
  %74 = load i8, i8* %67, align 1, !tbaa !3
  %75 = icmp eq i8 %74, 0
  br i1 %75, label %88, label %76

; <label>:76:                                     ; preds = %57
  br label %77

; <label>:77:                                     ; preds = %76, %77
  %78 = phi i8 [ %86, %77 ], [ %74, %76 ]
  %79 = phi i64 [ %85, %77 ], [ 0, %76 ]
  %80 = phi i8* [ %81, %77 ], [ %67, %76 ]
  %81 = getelementptr inbounds i8, i8* %80, i64 1
  %82 = and i8 %78, -64
  %83 = icmp ne i8 %82, -128
  %84 = zext i1 %83 to i64
  %85 = add i64 %79, %84
  %86 = load i8, i8* %81, align 1, !tbaa !3
  %87 = icmp eq i8 %86, 0
  br i1 %87, label %88, label %77

; <label>:88:                                     ; preds = %77, %57
  %89 = phi i64 [ 0, %57 ], [ %85, %77 ]
  store i64 %89, i64* %68, align 8, !tbaa !8
  ret %struct.String* %65
}

; Function Attrs: nounwind ssp uwtable
define %struct.Int* @lenStr(%struct.String* nocapture readonly) local_unnamed_addr #2 {
  %2 = getelementptr inbounds %struct.String, %struct.String* %0, i64 0, i32 0
  %3 = load i64, i64* %2, align 8, !tbaa !8
  %4 = tail call %struct.Int* @mkInt(i64 %3) #8
  ret %struct.Int* %4
}

declare %struct.Int* @mkInt(i64) local_unnamed_addr #5

; Function Attrs: nounwind ssp uwtable
define noalias %struct.String* @showInt(%struct.Int* nocapture readonly) local_unnamed_addr #2 {
  %2 = tail call i64 @llvm.objectsize.i64.p0i8(i8* null, i1 false, i1 true)
  %3 = getelementptr inbounds %struct.Int, %struct.Int* %0, i64 0, i32 0
  %4 = load i64, i64* %3, align 8, !tbaa !12
  %5 = tail call i32 (i8*, i64, i32, i64, i8*, ...) @__snprintf_chk(i8* null, i64 0, i32 0, i64 %2, i8* getelementptr inbounds ([5 x i8], [5 x i8]* @.str.2, i64 0, i64 0), i64 %4) #8
  %6 = add nsw i32 %5, 1
  %7 = sext i32 %6 to i64
  %8 = tail call i8* @malloc(i64 16) #10
  %9 = bitcast i8* %8 to %struct.String*
  %10 = add nsw i64 %7, 1
  %11 = tail call i8* @malloc(i64 %10) #10
  %12 = bitcast i8* %8 to i64*
  store i64 0, i64* %12, align 8, !tbaa !8
  %13 = getelementptr inbounds i8, i8* %8, i64 8
  %14 = bitcast i8* %13 to i8**
  store i8* %11, i8** %14, align 8, !tbaa !11
  %15 = tail call i64 @llvm.objectsize.i64.p0i8(i8* %11, i1 false, i1 true)
  %16 = load i64, i64* %3, align 8, !tbaa !12
  %17 = tail call i32 (i8*, i64, i32, i64, i8*, ...) @__snprintf_chk(i8* %11, i64 %7, i32 0, i64 %15, i8* getelementptr inbounds ([5 x i8], [5 x i8]* @.str.2, i64 0, i64 0), i64 %16) #8
  %18 = load i8, i8* %11, align 1, !tbaa !3
  %19 = icmp eq i8 %18, 0
  br i1 %19, label %32, label %20

; <label>:20:                                     ; preds = %1
  br label %21

; <label>:21:                                     ; preds = %20, %21
  %22 = phi i8 [ %30, %21 ], [ %18, %20 ]
  %23 = phi i64 [ %29, %21 ], [ 0, %20 ]
  %24 = phi i8* [ %25, %21 ], [ %11, %20 ]
  %25 = getelementptr inbounds i8, i8* %24, i64 1
  %26 = and i8 %22, -64
  %27 = icmp ne i8 %26, -128
  %28 = zext i1 %27 to i64
  %29 = add i64 %23, %28
  %30 = load i8, i8* %25, align 1, !tbaa !3
  %31 = icmp eq i8 %30, 0
  br i1 %31, label %32, label %21

; <label>:32:                                     ; preds = %21, %1
  %33 = phi i64 [ 0, %1 ], [ %29, %21 ]
  store i64 %33, i64* %12, align 8, !tbaa !8
  ret %struct.String* %9
}

; Function Attrs: nounwind ssp uwtable
define %struct.Int* @plusInt(%struct.Int* nocapture readonly, %struct.Int* nocapture readonly) local_unnamed_addr #2 {
  %3 = getelementptr inbounds %struct.Int, %struct.Int* %0, i64 0, i32 0
  %4 = load i64, i64* %3, align 8, !tbaa !12
  %5 = getelementptr inbounds %struct.Int, %struct.Int* %1, i64 0, i32 0
  %6 = load i64, i64* %5, align 8, !tbaa !12
  %7 = add i64 %6, %4
  %8 = tail call %struct.Int* @mkInt(i64 %7) #8
  ret %struct.Int* %8
}

; Function Attrs: nounwind ssp uwtable
define %struct.Int* @minusInt(%struct.Int* nocapture readonly, %struct.Int* nocapture readonly) local_unnamed_addr #2 {
  %3 = getelementptr inbounds %struct.Int, %struct.Int* %0, i64 0, i32 0
  %4 = load i64, i64* %3, align 8, !tbaa !12
  %5 = getelementptr inbounds %struct.Int, %struct.Int* %1, i64 0, i32 0
  %6 = load i64, i64* %5, align 8, !tbaa !12
  %7 = sub i64 %4, %6
  %8 = tail call %struct.Int* @mkInt(i64 %7) #8
  ret %struct.Int* %8
}

; Function Attrs: nounwind ssp uwtable
define %struct.Int* @multInt(%struct.Int* nocapture readonly, %struct.Int* nocapture readonly) local_unnamed_addr #2 {
  %3 = getelementptr inbounds %struct.Int, %struct.Int* %0, i64 0, i32 0
  %4 = load i64, i64* %3, align 8, !tbaa !12
  %5 = getelementptr inbounds %struct.Int, %struct.Int* %1, i64 0, i32 0
  %6 = load i64, i64* %5, align 8, !tbaa !12
  %7 = mul i64 %6, %4
  %8 = tail call %struct.Int* @mkInt(i64 %7) #8
  ret %struct.Int* %8
}

; Function Attrs: nounwind ssp uwtable
define %struct.Int* @divInt(%struct.Int* nocapture readonly, %struct.Int* nocapture readonly) local_unnamed_addr #2 {
  %3 = getelementptr inbounds %struct.Int, %struct.Int* %0, i64 0, i32 0
  %4 = load i64, i64* %3, align 8, !tbaa !12
  %5 = getelementptr inbounds %struct.Int, %struct.Int* %1, i64 0, i32 0
  %6 = load i64, i64* %5, align 8, !tbaa !12
  %7 = udiv i64 %4, %6
  %8 = tail call %struct.Int* @mkInt(i64 %7) #8
  ret %struct.Int* %8
}

; Function Attrs: nounwind ssp uwtable
define %struct.Int* @modInt(%struct.Int* nocapture readonly, %struct.Int* nocapture readonly) local_unnamed_addr #2 {
  %3 = getelementptr inbounds %struct.Int, %struct.Int* %0, i64 0, i32 0
  %4 = load i64, i64* %3, align 8, !tbaa !12
  %5 = getelementptr inbounds %struct.Int, %struct.Int* %1, i64 0, i32 0
  %6 = load i64, i64* %5, align 8, !tbaa !12
  %7 = urem i64 %4, %6
  %8 = tail call %struct.Int* @mkInt(i64 %7) #8
  ret %struct.Int* %8
}

; Function Attrs: nounwind ssp uwtable
define %struct.Bool* @ltInt(%struct.Int* nocapture readonly, %struct.Int* nocapture readonly) local_unnamed_addr #2 {
  %3 = getelementptr inbounds %struct.Int, %struct.Int* %0, i64 0, i32 0
  %4 = load i64, i64* %3, align 8, !tbaa !12
  %5 = getelementptr inbounds %struct.Int, %struct.Int* %1, i64 0, i32 0
  %6 = load i64, i64* %5, align 8, !tbaa !12
  %7 = icmp ult i64 %4, %6
  br i1 %7, label %8, label %10

; <label>:8:                                      ; preds = %2
  %9 = tail call %struct.Bool* (...) @True() #8
  br label %12

; <label>:10:                                     ; preds = %2
  %11 = tail call %struct.Bool* (...) @False() #8
  br label %12

; <label>:12:                                     ; preds = %10, %8
  %13 = phi %struct.Bool* [ %9, %8 ], [ %11, %10 ]
  ret %struct.Bool* %13
}

declare %struct.Bool* @True(...) local_unnamed_addr #5

declare %struct.Bool* @False(...) local_unnamed_addr #5

; Function Attrs: nounwind ssp uwtable
define %struct.Bool* @gtInt(%struct.Int* nocapture readonly, %struct.Int* nocapture readonly) local_unnamed_addr #2 {
  %3 = getelementptr inbounds %struct.Int, %struct.Int* %0, i64 0, i32 0
  %4 = load i64, i64* %3, align 8, !tbaa !12
  %5 = getelementptr inbounds %struct.Int, %struct.Int* %1, i64 0, i32 0
  %6 = load i64, i64* %5, align 8, !tbaa !12
  %7 = icmp ugt i64 %4, %6
  br i1 %7, label %8, label %10

; <label>:8:                                      ; preds = %2
  %9 = tail call %struct.Bool* (...) @True() #8
  br label %12

; <label>:10:                                     ; preds = %2
  %11 = tail call %struct.Bool* (...) @False() #8
  br label %12

; <label>:12:                                     ; preds = %10, %8
  %13 = phi %struct.Bool* [ %9, %8 ], [ %11, %10 ]
  ret %struct.Bool* %13
}

; Function Attrs: nounwind ssp uwtable
define %struct.Bool* @eqInt(%struct.Int* nocapture readonly, %struct.Int* nocapture readonly) local_unnamed_addr #2 {
  %3 = getelementptr inbounds %struct.Int, %struct.Int* %0, i64 0, i32 0
  %4 = load i64, i64* %3, align 8, !tbaa !12
  %5 = getelementptr inbounds %struct.Int, %struct.Int* %1, i64 0, i32 0
  %6 = load i64, i64* %5, align 8, !tbaa !12
  %7 = icmp eq i64 %4, %6
  br i1 %7, label %8, label %10

; <label>:8:                                      ; preds = %2
  %9 = tail call %struct.Bool* (...) @True() #8
  br label %12

; <label>:10:                                     ; preds = %2
  %11 = tail call %struct.Bool* (...) @False() #8
  br label %12

; <label>:12:                                     ; preds = %10, %8
  %13 = phi %struct.Bool* [ %9, %8 ], [ %11, %10 ]
  ret %struct.Bool* %13
}

; Function Attrs: nounwind ssp uwtable
define %struct.Bool* @leqInt(%struct.Int* nocapture readonly, %struct.Int* nocapture readonly) local_unnamed_addr #2 {
  %3 = getelementptr inbounds %struct.Int, %struct.Int* %0, i64 0, i32 0
  %4 = load i64, i64* %3, align 8, !tbaa !12
  %5 = getelementptr inbounds %struct.Int, %struct.Int* %1, i64 0, i32 0
  %6 = load i64, i64* %5, align 8, !tbaa !12
  %7 = icmp ugt i64 %4, %6
  br i1 %7, label %10, label %8

; <label>:8:                                      ; preds = %2
  %9 = tail call %struct.Bool* (...) @True() #8
  br label %12

; <label>:10:                                     ; preds = %2
  %11 = tail call %struct.Bool* (...) @False() #8
  br label %12

; <label>:12:                                     ; preds = %10, %8
  %13 = phi %struct.Bool* [ %9, %8 ], [ %11, %10 ]
  ret %struct.Bool* %13
}

; Function Attrs: nounwind ssp uwtable
define %struct.Bool* @geqInt(%struct.Int* nocapture readonly, %struct.Int* nocapture readonly) local_unnamed_addr #2 {
  %3 = getelementptr inbounds %struct.Int, %struct.Int* %0, i64 0, i32 0
  %4 = load i64, i64* %3, align 8, !tbaa !12
  %5 = getelementptr inbounds %struct.Int, %struct.Int* %1, i64 0, i32 0
  %6 = load i64, i64* %5, align 8, !tbaa !12
  %7 = icmp ult i64 %4, %6
  br i1 %7, label %10, label %8

; <label>:8:                                      ; preds = %2
  %9 = tail call %struct.Bool* (...) @True() #8
  br label %12

; <label>:10:                                     ; preds = %2
  %11 = tail call %struct.Bool* (...) @False() #8
  br label %12

; <label>:12:                                     ; preds = %10, %8
  %13 = phi %struct.Bool* [ %9, %8 ], [ %11, %10 ]
  ret %struct.Bool* %13
}

; Function Attrs: nounwind ssp uwtable
define noalias %struct.String* @showDouble(%struct.Double* nocapture readonly) local_unnamed_addr #2 {
  %2 = tail call i64 @llvm.objectsize.i64.p0i8(i8* null, i1 false, i1 true)
  %3 = getelementptr inbounds %struct.Double, %struct.Double* %0, i64 0, i32 0
  %4 = load double, double* %3, align 8, !tbaa !15
  %5 = tail call i32 (i8*, i64, i32, i64, i8*, ...) @__snprintf_chk(i8* null, i64 0, i32 0, i64 %2, i8* getelementptr inbounds ([3 x i8], [3 x i8]* @.str.3, i64 0, i64 0), double %4) #8
  %6 = add nsw i32 %5, 1
  %7 = sext i32 %6 to i64
  %8 = tail call i8* @malloc(i64 16) #10
  %9 = bitcast i8* %8 to %struct.String*
  %10 = add nsw i64 %7, 1
  %11 = tail call i8* @malloc(i64 %10) #10
  %12 = bitcast i8* %8 to i64*
  store i64 0, i64* %12, align 8, !tbaa !8
  %13 = getelementptr inbounds i8, i8* %8, i64 8
  %14 = bitcast i8* %13 to i8**
  store i8* %11, i8** %14, align 8, !tbaa !11
  %15 = tail call i64 @llvm.objectsize.i64.p0i8(i8* %11, i1 false, i1 true)
  %16 = load double, double* %3, align 8, !tbaa !15
  %17 = tail call i32 (i8*, i64, i32, i64, i8*, ...) @__snprintf_chk(i8* %11, i64 %7, i32 0, i64 %15, i8* getelementptr inbounds ([3 x i8], [3 x i8]* @.str.3, i64 0, i64 0), double %16) #8
  %18 = load i8, i8* %11, align 1, !tbaa !3
  %19 = icmp eq i8 %18, 0
  br i1 %19, label %32, label %20

; <label>:20:                                     ; preds = %1
  br label %21

; <label>:21:                                     ; preds = %20, %21
  %22 = phi i8 [ %30, %21 ], [ %18, %20 ]
  %23 = phi i64 [ %29, %21 ], [ 0, %20 ]
  %24 = phi i8* [ %25, %21 ], [ %11, %20 ]
  %25 = getelementptr inbounds i8, i8* %24, i64 1
  %26 = and i8 %22, -64
  %27 = icmp ne i8 %26, -128
  %28 = zext i1 %27 to i64
  %29 = add i64 %23, %28
  %30 = load i8, i8* %25, align 1, !tbaa !3
  %31 = icmp eq i8 %30, 0
  br i1 %31, label %32, label %21

; <label>:32:                                     ; preds = %21, %1
  %33 = phi i64 [ 0, %1 ], [ %29, %21 ]
  store i64 %33, i64* %12, align 8, !tbaa !8
  ret %struct.String* %9
}

; Function Attrs: nounwind ssp uwtable
define %struct.Double* @plusDouble(%struct.Double* nocapture readonly, %struct.Double* nocapture readonly) local_unnamed_addr #2 {
  %3 = getelementptr inbounds %struct.Double, %struct.Double* %0, i64 0, i32 0
  %4 = load double, double* %3, align 8, !tbaa !15
  %5 = getelementptr inbounds %struct.Double, %struct.Double* %1, i64 0, i32 0
  %6 = load double, double* %5, align 8, !tbaa !15
  %7 = fadd double %4, %6
  %8 = tail call %struct.Double* @mkDouble(double %7) #8
  ret %struct.Double* %8
}

declare %struct.Double* @mkDouble(double) local_unnamed_addr #5

; Function Attrs: nounwind ssp uwtable
define %struct.Double* @minusDouble(%struct.Double* nocapture readonly, %struct.Double* nocapture readonly) local_unnamed_addr #2 {
  %3 = getelementptr inbounds %struct.Double, %struct.Double* %0, i64 0, i32 0
  %4 = load double, double* %3, align 8, !tbaa !15
  %5 = getelementptr inbounds %struct.Double, %struct.Double* %1, i64 0, i32 0
  %6 = load double, double* %5, align 8, !tbaa !15
  %7 = fsub double %4, %6
  %8 = tail call %struct.Double* @mkDouble(double %7) #8
  ret %struct.Double* %8
}

; Function Attrs: nounwind ssp uwtable
define %struct.Double* @multDouble(%struct.Double* nocapture readonly, %struct.Double* nocapture readonly) local_unnamed_addr #2 {
  %3 = getelementptr inbounds %struct.Double, %struct.Double* %0, i64 0, i32 0
  %4 = load double, double* %3, align 8, !tbaa !15
  %5 = getelementptr inbounds %struct.Double, %struct.Double* %1, i64 0, i32 0
  %6 = load double, double* %5, align 8, !tbaa !15
  %7 = fmul double %4, %6
  %8 = tail call %struct.Double* @mkDouble(double %7) #8
  ret %struct.Double* %8
}

; Function Attrs: nounwind ssp uwtable
define %struct.Double* @divDouble(%struct.Double* nocapture readonly, %struct.Double* nocapture readonly) local_unnamed_addr #2 {
  %3 = getelementptr inbounds %struct.Double, %struct.Double* %0, i64 0, i32 0
  %4 = load double, double* %3, align 8, !tbaa !15
  %5 = getelementptr inbounds %struct.Double, %struct.Double* %1, i64 0, i32 0
  %6 = load double, double* %5, align 8, !tbaa !15
  %7 = fdiv double %4, %6
  %8 = tail call %struct.Double* @mkDouble(double %7) #8
  ret %struct.Double* %8
}

; Function Attrs: nounwind ssp uwtable
define %struct.Double* @modDouble(%struct.Double* nocapture readonly, %struct.Double* nocapture readonly) local_unnamed_addr #2 {
  %3 = getelementptr inbounds %struct.Double, %struct.Double* %0, i64 0, i32 0
  %4 = load double, double* %3, align 8, !tbaa !15
  %5 = getelementptr inbounds %struct.Double, %struct.Double* %1, i64 0, i32 0
  %6 = load double, double* %5, align 8, !tbaa !15
  %7 = tail call double @fmod(double %4, double %6) #11
  %8 = tail call %struct.Double* @mkDouble(double %7) #8
  ret %struct.Double* %8
}

; Function Attrs: nounwind readnone
declare double @fmod(double, double) local_unnamed_addr #7

; Function Attrs: nounwind ssp uwtable
define %struct.Bool* @ltDouble(%struct.Double* nocapture readonly, %struct.Double* nocapture readonly) local_unnamed_addr #2 {
  %3 = getelementptr inbounds %struct.Double, %struct.Double* %0, i64 0, i32 0
  %4 = load double, double* %3, align 8, !tbaa !15
  %5 = getelementptr inbounds %struct.Double, %struct.Double* %1, i64 0, i32 0
  %6 = load double, double* %5, align 8, !tbaa !15
  %7 = fcmp olt double %4, %6
  br i1 %7, label %8, label %10

; <label>:8:                                      ; preds = %2
  %9 = tail call %struct.Bool* (...) @True() #8
  br label %12

; <label>:10:                                     ; preds = %2
  %11 = tail call %struct.Bool* (...) @False() #8
  br label %12

; <label>:12:                                     ; preds = %10, %8
  %13 = phi %struct.Bool* [ %9, %8 ], [ %11, %10 ]
  ret %struct.Bool* %13
}

; Function Attrs: nounwind ssp uwtable
define %struct.Bool* @gtDouble(%struct.Double* nocapture readonly, %struct.Double* nocapture readonly) local_unnamed_addr #2 {
  %3 = getelementptr inbounds %struct.Double, %struct.Double* %0, i64 0, i32 0
  %4 = load double, double* %3, align 8, !tbaa !15
  %5 = getelementptr inbounds %struct.Double, %struct.Double* %1, i64 0, i32 0
  %6 = load double, double* %5, align 8, !tbaa !15
  %7 = fcmp ogt double %4, %6
  br i1 %7, label %8, label %10

; <label>:8:                                      ; preds = %2
  %9 = tail call %struct.Bool* (...) @True() #8
  br label %12

; <label>:10:                                     ; preds = %2
  %11 = tail call %struct.Bool* (...) @False() #8
  br label %12

; <label>:12:                                     ; preds = %10, %8
  %13 = phi %struct.Bool* [ %9, %8 ], [ %11, %10 ]
  ret %struct.Bool* %13
}

; Function Attrs: nounwind ssp uwtable
define %struct.Bool* @eqDouble(%struct.Double* nocapture readonly, %struct.Double* nocapture readonly) local_unnamed_addr #2 {
  %3 = getelementptr inbounds %struct.Double, %struct.Double* %0, i64 0, i32 0
  %4 = load double, double* %3, align 8, !tbaa !15
  %5 = getelementptr inbounds %struct.Double, %struct.Double* %1, i64 0, i32 0
  %6 = load double, double* %5, align 8, !tbaa !15
  %7 = fcmp oeq double %4, %6
  br i1 %7, label %8, label %10

; <label>:8:                                      ; preds = %2
  %9 = tail call %struct.Bool* (...) @True() #8
  br label %12

; <label>:10:                                     ; preds = %2
  %11 = tail call %struct.Bool* (...) @False() #8
  br label %12

; <label>:12:                                     ; preds = %10, %8
  %13 = phi %struct.Bool* [ %9, %8 ], [ %11, %10 ]
  ret %struct.Bool* %13
}

; Function Attrs: nounwind ssp uwtable
define %struct.Bool* @leqDouble(%struct.Double* nocapture readonly, %struct.Double* nocapture readonly) local_unnamed_addr #2 {
  %3 = getelementptr inbounds %struct.Double, %struct.Double* %0, i64 0, i32 0
  %4 = load double, double* %3, align 8, !tbaa !15
  %5 = getelementptr inbounds %struct.Double, %struct.Double* %1, i64 0, i32 0
  %6 = load double, double* %5, align 8, !tbaa !15
  %7 = fcmp ugt double %4, %6
  br i1 %7, label %10, label %8

; <label>:8:                                      ; preds = %2
  %9 = tail call %struct.Bool* (...) @True() #8
  br label %12

; <label>:10:                                     ; preds = %2
  %11 = tail call %struct.Bool* (...) @False() #8
  br label %12

; <label>:12:                                     ; preds = %10, %8
  %13 = phi %struct.Bool* [ %9, %8 ], [ %11, %10 ]
  ret %struct.Bool* %13
}

; Function Attrs: nounwind ssp uwtable
define %struct.Bool* @geqDouble(%struct.Double* nocapture readonly, %struct.Double* nocapture readonly) local_unnamed_addr #2 {
  %3 = getelementptr inbounds %struct.Double, %struct.Double* %0, i64 0, i32 0
  %4 = load double, double* %3, align 8, !tbaa !15
  %5 = getelementptr inbounds %struct.Double, %struct.Double* %1, i64 0, i32 0
  %6 = load double, double* %5, align 8, !tbaa !15
  %7 = fcmp ult double %4, %6
  br i1 %7, label %10, label %8

; <label>:8:                                      ; preds = %2
  %9 = tail call %struct.Bool* (...) @True() #8
  br label %12

; <label>:10:                                     ; preds = %2
  %11 = tail call %struct.Bool* (...) @False() #8
  br label %12

; <label>:12:                                     ; preds = %10, %8
  %13 = phi %struct.Bool* [ %9, %8 ], [ %11, %10 ]
  ret %struct.Bool* %13
}

; Function Attrs: nounwind ssp uwtable
define %struct.String* @omgDebug(%struct.String* readonly returned) local_unnamed_addr #2 {
  %2 = getelementptr inbounds %struct.String, %struct.String* %0, i64 0, i32 1
  %3 = load i8*, i8** %2, align 8, !tbaa !11
  %4 = tail call i32 @puts(i8* %3)
  ret %struct.String* %0
}

; Function Attrs: nounwind ssp uwtable
define i32 @main() local_unnamed_addr #2 {
  %1 = tail call %struct.String* (...) @module_main() #8
  %2 = getelementptr inbounds %struct.String, %struct.String* %1, i64 0, i32 1
  %3 = load i8*, i8** %2, align 8, !tbaa !11
  %4 = tail call i32 @puts(i8* %3)
  ret i32 0
}

declare %struct.String* @module_main(...) local_unnamed_addr #5

; Function Attrs: nounwind
declare i32 @puts(i8* nocapture readonly) #8

attributes #0 = { norecurse nounwind readonly ssp uwtable "correctly-rounded-divide-sqrt-fp-math"="false" "disable-tail-calls"="false" "less-precise-fpmad"="false" "no-frame-pointer-elim"="true" "no-frame-pointer-elim-non-leaf" "no-infs-fp-math"="false" "no-jump-tables"="false" "no-nans-fp-math"="false" "no-signed-zeros-fp-math"="false" "no-trapping-math"="false" "stack-protector-buffer-size"="8" "target-cpu"="penryn" "target-features"="+cx16,+fxsr,+mmx,+sse,+sse2,+sse3,+sse4.1,+ssse3,+x87" "unsafe-fp-math"="false" "use-soft-float"="false" }
attributes #1 = { norecurse nounwind ssp uwtable "correctly-rounded-divide-sqrt-fp-math"="false" "disable-tail-calls"="false" "less-precise-fpmad"="false" "no-frame-pointer-elim"="true" "no-frame-pointer-elim-non-leaf" "no-infs-fp-math"="false" "no-jump-tables"="false" "no-nans-fp-math"="false" "no-signed-zeros-fp-math"="false" "no-trapping-math"="false" "stack-protector-buffer-size"="8" "target-cpu"="penryn" "target-features"="+cx16,+fxsr,+mmx,+sse,+sse2,+sse3,+sse4.1,+ssse3,+x87" "unsafe-fp-math"="false" "use-soft-float"="false" }
attributes #2 = { nounwind ssp uwtable "correctly-rounded-divide-sqrt-fp-math"="false" "disable-tail-calls"="false" "less-precise-fpmad"="false" "no-frame-pointer-elim"="true" "no-frame-pointer-elim-non-leaf" "no-infs-fp-math"="false" "no-jump-tables"="false" "no-nans-fp-math"="false" "no-signed-zeros-fp-math"="false" "no-trapping-math"="false" "stack-protector-buffer-size"="8" "target-cpu"="penryn" "target-features"="+cx16,+fxsr,+mmx,+sse,+sse2,+sse3,+sse4.1,+ssse3,+x87" "unsafe-fp-math"="false" "use-soft-float"="false" }
attributes #3 = { nounwind allocsize(0) "correctly-rounded-divide-sqrt-fp-math"="false" "disable-tail-calls"="false" "less-precise-fpmad"="false" "no-frame-pointer-elim"="true" "no-frame-pointer-elim-non-leaf" "no-infs-fp-math"="false" "no-nans-fp-math"="false" "no-signed-zeros-fp-math"="false" "no-trapping-math"="false" "stack-protector-buffer-size"="8" "target-cpu"="penryn" "target-features"="+cx16,+fxsr,+mmx,+sse,+sse2,+sse3,+sse4.1,+ssse3,+x87" "unsafe-fp-math"="false" "use-soft-float"="false" }
attributes #4 = { argmemonly nounwind readonly "correctly-rounded-divide-sqrt-fp-math"="false" "disable-tail-calls"="false" "less-precise-fpmad"="false" "no-frame-pointer-elim"="true" "no-frame-pointer-elim-non-leaf" "no-infs-fp-math"="false" "no-nans-fp-math"="false" "no-signed-zeros-fp-math"="false" "no-trapping-math"="false" "stack-protector-buffer-size"="8" "target-cpu"="penryn" "target-features"="+cx16,+fxsr,+mmx,+sse,+sse2,+sse3,+sse4.1,+ssse3,+x87" "unsafe-fp-math"="false" "use-soft-float"="false" }
attributes #5 = { "correctly-rounded-divide-sqrt-fp-math"="false" "disable-tail-calls"="false" "less-precise-fpmad"="false" "no-frame-pointer-elim"="true" "no-frame-pointer-elim-non-leaf" "no-infs-fp-math"="false" "no-nans-fp-math"="false" "no-signed-zeros-fp-math"="false" "no-trapping-math"="false" "stack-protector-buffer-size"="8" "target-cpu"="penryn" "target-features"="+cx16,+fxsr,+mmx,+sse,+sse2,+sse3,+sse4.1,+ssse3,+x87" "unsafe-fp-math"="false" "use-soft-float"="false" }
attributes #6 = { nounwind readnone speculatable }
attributes #7 = { nounwind readnone "correctly-rounded-divide-sqrt-fp-math"="false" "disable-tail-calls"="false" "less-precise-fpmad"="false" "no-frame-pointer-elim"="true" "no-frame-pointer-elim-non-leaf" "no-infs-fp-math"="false" "no-nans-fp-math"="false" "no-signed-zeros-fp-math"="false" "no-trapping-math"="false" "stack-protector-buffer-size"="8" "target-cpu"="penryn" "target-features"="+cx16,+fxsr,+mmx,+sse,+sse2,+sse3,+sse4.1,+ssse3,+x87" "unsafe-fp-math"="false" "use-soft-float"="false" }
attributes #8 = { nounwind }
attributes #9 = { allocsize(0) }
attributes #10 = { nounwind allocsize(0) }
attributes #11 = { nounwind readnone }

!llvm.module.flags = !{!0, !1}
!llvm.ident = !{!2}

!0 = !{i32 1, !"wchar_size", i32 4}
!1 = !{i32 7, !"PIC Level", i32 2}
!2 = !{!"Apple LLVM version 9.1.0 (clang-902.0.39.2)"}
!3 = !{!4, !4, i64 0}
!4 = !{!"omnipotent char", !5, i64 0}
!5 = !{!"Simple C/C++ TBAA"}
!6 = !{!7, !7, i64 0}
!7 = !{!"long", !4, i64 0}
!8 = !{!9, !7, i64 0}
!9 = !{!"", !7, i64 0, !10, i64 8}
!10 = !{!"any pointer", !4, i64 0}
!11 = !{!9, !10, i64 8}
!12 = !{!13, !14, i64 0}
!13 = !{!"", !14, i64 0}
!14 = !{!"long long", !4, i64 0}
!15 = !{!16, !17, i64 0}
!16 = !{!"", !17, i64 0}
!17 = !{!"double", !4, i64 0}
