; ModuleID = 'rts.c'
source_filename = "rts.c"
target datalayout = "e-m:o-i64:64-f80:128-n8:16:32:64-S128"
target triple = "x86_64-apple-macosx10.13.0"

%struct.String = type { i64, i8* }
%struct.Int = type { i64, i64 }

@.str = private unnamed_addr constant [5 x i8] c"%lld\00", align 1
@.str.1 = private unnamed_addr constant [5 x i8] c"%s%s\00", align 1

; Function Attrs: nounwind ssp uwtable
define noalias %struct.String* @showInt(%struct.Int* nocapture readonly %x) local_unnamed_addr #0 {
entry:
  %0 = tail call i64 @llvm.objectsize.i64.p0i8(i8* null, i1 false, i1 true)
  %val = getelementptr inbounds %struct.Int, %struct.Int* %x, i64 0, i32 1
  %1 = load i64, i64* %val, align 8, !tbaa !3
  %call = tail call i32 (i8*, i64, i32, i64, i8*, ...) @__snprintf_chk(i8* null, i64 0, i32 0, i64 %0, i8* getelementptr inbounds ([5 x i8], [5 x i8]* @.str, i64 0, i64 0), i64 %1) #4
  %add = add nsw i32 %call, 1
  %conv = sext i32 %add to i64
  %call.i = tail call i8* @malloc(i64 16) #5
  %2 = bitcast i8* %call.i to %struct.String*
  %add.i = add nsw i64 %conv, 1
  %call1.i = tail call i8* @malloc(i64 %add.i) #5
  %string_length.i = bitcast i8* %call.i to i64*
  store i64 %conv, i64* %string_length.i, align 8, !tbaa !8
  %data.i = getelementptr inbounds i8, i8* %call.i, i64 8
  %3 = bitcast i8* %data.i to i8**
  store i8* %call1.i, i8** %3, align 8, !tbaa !12
  %4 = tail call i64 @llvm.objectsize.i64.p0i8(i8* %call1.i, i1 false, i1 true)
  %5 = load i64, i64* %val, align 8, !tbaa !3
  %call5 = tail call i32 (i8*, i64, i32, i64, i8*, ...) @__snprintf_chk(i8* %call1.i, i64 %conv, i32 0, i64 %4, i8* getelementptr inbounds ([5 x i8], [5 x i8]* @.str, i64 0, i64 0), i64 %5) #4
  ret %struct.String* %2
}

declare i32 @__snprintf_chk(i8*, i64, i32, i64, i8*, ...) local_unnamed_addr #1

; Function Attrs: nounwind readnone speculatable
declare i64 @llvm.objectsize.i64.p0i8(i8*, i1, i1) #2

; Function Attrs: nounwind ssp uwtable
define noalias %struct.String* @mkString(i64 %length) local_unnamed_addr #0 {
entry:
  %call = tail call i8* @malloc(i64 16) #6
  %0 = bitcast i8* %call to %struct.String*
  %add = add i64 %length, 1
  %call1 = tail call i8* @malloc(i64 %add) #6
  %string_length = bitcast i8* %call to i64*
  store i64 %length, i64* %string_length, align 8, !tbaa !8
  %data = getelementptr inbounds i8, i8* %call, i64 8
  %1 = bitcast i8* %data to i8**
  store i8* %call1, i8** %1, align 8, !tbaa !12
  ret %struct.String* %0
}

; Function Attrs: nounwind allocsize(0)
declare noalias i8* @malloc(i64) local_unnamed_addr #3

; Function Attrs: nounwind ssp uwtable
define %struct.Int* @plusInt(%struct.Int* nocapture readonly %a, %struct.Int* nocapture readonly %b) local_unnamed_addr #0 {
entry:
  %val = getelementptr inbounds %struct.Int, %struct.Int* %a, i64 0, i32 1
  %0 = load i64, i64* %val, align 8, !tbaa !3
  %val1 = getelementptr inbounds %struct.Int, %struct.Int* %b, i64 0, i32 1
  %1 = load i64, i64* %val1, align 8, !tbaa !3
  %add = add i64 %1, %0
  %call = tail call %struct.Int* @mkInt(i64 %add) #4
  ret %struct.Int* %call
}

declare %struct.Int* @mkInt(i64) local_unnamed_addr #1

; Function Attrs: nounwind ssp uwtable
define %struct.Int* @minusInt(%struct.Int* nocapture readonly %a, %struct.Int* nocapture readonly %b) local_unnamed_addr #0 {
entry:
  %val = getelementptr inbounds %struct.Int, %struct.Int* %a, i64 0, i32 1
  %0 = load i64, i64* %val, align 8, !tbaa !3
  %val1 = getelementptr inbounds %struct.Int, %struct.Int* %b, i64 0, i32 1
  %1 = load i64, i64* %val1, align 8, !tbaa !3
  %sub = sub i64 %0, %1
  %call = tail call %struct.Int* @mkInt(i64 %sub) #4
  ret %struct.Int* %call
}

; Function Attrs: nounwind ssp uwtable
define noalias %struct.String* @plusStr(%struct.String* nocapture readonly %a, %struct.String* nocapture readonly %b) local_unnamed_addr #0 {
entry:
  %string_length = getelementptr inbounds %struct.String, %struct.String* %a, i64 0, i32 0
  %0 = load i64, i64* %string_length, align 8, !tbaa !8
  %string_length1 = getelementptr inbounds %struct.String, %struct.String* %b, i64 0, i32 0
  %1 = load i64, i64* %string_length1, align 8, !tbaa !8
  %add = add i64 %1, %0
  %sext = shl i64 %add, 32
  %conv2 = ashr exact i64 %sext, 32
  %call.i = tail call i8* @malloc(i64 16) #5
  %2 = bitcast i8* %call.i to %struct.String*
  %add.i = add nsw i64 %conv2, 1
  %call1.i = tail call i8* @malloc(i64 %add.i) #5
  %string_length.i = bitcast i8* %call.i to i64*
  store i64 %conv2, i64* %string_length.i, align 8, !tbaa !8
  %data.i = getelementptr inbounds i8, i8* %call.i, i64 8
  %3 = bitcast i8* %data.i to i8**
  store i8* %call1.i, i8** %3, align 8, !tbaa !12
  %sext16 = add i64 %sext, 4294967296
  %conv4 = ashr exact i64 %sext16, 32
  %4 = tail call i64 @llvm.objectsize.i64.p0i8(i8* %call1.i, i1 false, i1 true)
  %data6 = getelementptr inbounds %struct.String, %struct.String* %a, i64 0, i32 1
  %5 = load i8*, i8** %data6, align 8, !tbaa !12
  %data7 = getelementptr inbounds %struct.String, %struct.String* %b, i64 0, i32 1
  %6 = load i8*, i8** %data7, align 8, !tbaa !12
  %call8 = tail call i32 (i8*, i64, i32, i64, i8*, ...) @__snprintf_chk(i8* %call1.i, i64 %conv4, i32 0, i64 %4, i8* getelementptr inbounds ([5 x i8], [5 x i8]* @.str.1, i64 0, i64 0), i8* %5, i8* %6) #4
  ret %struct.String* %2
}

; Function Attrs: nounwind ssp uwtable
define %struct.String* @omgDebug(%struct.String* readonly returned %a) local_unnamed_addr #0 {
entry:
  %data = getelementptr inbounds %struct.String, %struct.String* %a, i64 0, i32 1
  %0 = load i8*, i8** %data, align 8, !tbaa !12
  %puts = tail call i32 @puts(i8* %0)
  ret %struct.String* %a
}

; Function Attrs: nounwind ssp uwtable
define i32 @main() local_unnamed_addr #0 {
entry:
  %call = tail call %struct.String* (...) @module_main() #4
  %data = getelementptr inbounds %struct.String, %struct.String* %call, i64 0, i32 1
  %0 = load i8*, i8** %data, align 8, !tbaa !12
  %puts = tail call i32 @puts(i8* %0)
  ret i32 0
}

declare %struct.String* @module_main(...) local_unnamed_addr #1

; Function Attrs: nounwind
declare i32 @puts(i8* nocapture readonly) #4

attributes #0 = { nounwind ssp uwtable "correctly-rounded-divide-sqrt-fp-math"="false" "disable-tail-calls"="false" "less-precise-fpmad"="false" "no-frame-pointer-elim"="true" "no-frame-pointer-elim-non-leaf" "no-infs-fp-math"="false" "no-jump-tables"="false" "no-nans-fp-math"="false" "no-signed-zeros-fp-math"="false" "no-trapping-math"="false" "stack-protector-buffer-size"="8" "target-cpu"="penryn" "target-features"="+cx16,+fxsr,+mmx,+sse,+sse2,+sse3,+sse4.1,+ssse3,+x87" "unsafe-fp-math"="false" "use-soft-float"="false" }
attributes #1 = { "correctly-rounded-divide-sqrt-fp-math"="false" "disable-tail-calls"="false" "less-precise-fpmad"="false" "no-frame-pointer-elim"="true" "no-frame-pointer-elim-non-leaf" "no-infs-fp-math"="false" "no-nans-fp-math"="false" "no-signed-zeros-fp-math"="false" "no-trapping-math"="false" "stack-protector-buffer-size"="8" "target-cpu"="penryn" "target-features"="+cx16,+fxsr,+mmx,+sse,+sse2,+sse3,+sse4.1,+ssse3,+x87" "unsafe-fp-math"="false" "use-soft-float"="false" }
attributes #2 = { nounwind readnone speculatable }
attributes #3 = { nounwind allocsize(0) "correctly-rounded-divide-sqrt-fp-math"="false" "disable-tail-calls"="false" "less-precise-fpmad"="false" "no-frame-pointer-elim"="true" "no-frame-pointer-elim-non-leaf" "no-infs-fp-math"="false" "no-nans-fp-math"="false" "no-signed-zeros-fp-math"="false" "no-trapping-math"="false" "stack-protector-buffer-size"="8" "target-cpu"="penryn" "target-features"="+cx16,+fxsr,+mmx,+sse,+sse2,+sse3,+sse4.1,+ssse3,+x87" "unsafe-fp-math"="false" "use-soft-float"="false" }
attributes #4 = { nounwind }
attributes #5 = { nounwind allocsize(0) }
attributes #6 = { allocsize(0) }

!llvm.module.flags = !{!0, !1}
!llvm.ident = !{!2}

!0 = !{i32 1, !"wchar_size", i32 4}
!1 = !{i32 7, !"PIC Level", i32 2}
!2 = !{!"clang version 5.0.0 (tags/RELEASE_500/final)"}
!3 = !{!4, !5, i64 8}
!4 = !{!"", !5, i64 0, !5, i64 8}
!5 = !{!"long long", !6, i64 0}
!6 = !{!"omnipotent char", !7, i64 0}
!7 = !{!"Simple C/C++ TBAA"}
!8 = !{!9, !10, i64 0}
!9 = !{!"", !10, i64 0, !11, i64 8}
!10 = !{!"long", !6, i64 0}
!11 = !{!"any pointer", !6, i64 0}
!12 = !{!9, !11, i64 8}
