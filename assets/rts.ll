; ModuleID = 'rts.c'
source_filename = "rts.c"
target datalayout = "e-m:o-i64:64-f80:128-n8:16:32:64-S128"
target triple = "x86_64-apple-macosx10.13.0"

%struct.String = type { i64, i8* }
%struct.Int = type { i64, i64 }

@.str = private unnamed_addr constant [5 x i8] c"%lld\00", align 1
@.str.1 = private unnamed_addr constant [5 x i8] c"%s%s\00", align 1
@.str.2 = private unnamed_addr constant [4 x i8] c"%s\0A\00", align 1

; Function Attrs: noinline nounwind optnone ssp uwtable
define %struct.String* @showInt(%struct.Int* %x) #0 {
entry:
  %x.addr = alloca %struct.Int*, align 8
  %num_chars = alloca i32, align 4
  %str = alloca %struct.String*, align 8
  store %struct.Int* %x, %struct.Int** %x.addr, align 8
  %0 = call i64 @llvm.objectsize.i64.p0i8(i8* null, i1 false, i1 true)
  %1 = load %struct.Int*, %struct.Int** %x.addr, align 8
  %val = getelementptr inbounds %struct.Int, %struct.Int* %1, i32 0, i32 1
  %2 = load i64, i64* %val, align 8
  %call = call i32 (i8*, i64, i32, i64, i8*, ...) @__snprintf_chk(i8* null, i64 0, i32 0, i64 %0, i8* getelementptr inbounds ([5 x i8], [5 x i8]* @.str, i32 0, i32 0), i64 %2)
  %add = add nsw i32 %call, 1
  store i32 %add, i32* %num_chars, align 4
  %3 = load i32, i32* %num_chars, align 4
  %conv = sext i32 %3 to i64
  %call1 = call %struct.String* @mkString(i64 %conv)
  store %struct.String* %call1, %struct.String** %str, align 8
  %4 = load %struct.String*, %struct.String** %str, align 8
  %data = getelementptr inbounds %struct.String, %struct.String* %4, i32 0, i32 1
  %5 = load i8*, i8** %data, align 8
  %6 = load i32, i32* %num_chars, align 4
  %conv2 = sext i32 %6 to i64
  %7 = load %struct.String*, %struct.String** %str, align 8
  %data3 = getelementptr inbounds %struct.String, %struct.String* %7, i32 0, i32 1
  %8 = load i8*, i8** %data3, align 8
  %9 = call i64 @llvm.objectsize.i64.p0i8(i8* %8, i1 false, i1 true)
  %10 = load %struct.Int*, %struct.Int** %x.addr, align 8
  %val4 = getelementptr inbounds %struct.Int, %struct.Int* %10, i32 0, i32 1
  %11 = load i64, i64* %val4, align 8
  %call5 = call i32 (i8*, i64, i32, i64, i8*, ...) @__snprintf_chk(i8* %5, i64 %conv2, i32 0, i64 %9, i8* getelementptr inbounds ([5 x i8], [5 x i8]* @.str, i32 0, i32 0), i64 %11)
  %12 = load %struct.String*, %struct.String** %str, align 8
  ret %struct.String* %12
}

declare i32 @__snprintf_chk(i8*, i64, i32, i64, i8*, ...) #1

; Function Attrs: nounwind readnone speculatable
declare i64 @llvm.objectsize.i64.p0i8(i8*, i1, i1) #2

; Function Attrs: noinline nounwind optnone ssp uwtable
define %struct.String* @mkString(i64 %length) #0 {
entry:
  %length.addr = alloca i64, align 8
  %strPtr = alloca %struct.String*, align 8
  %buffer = alloca i8*, align 8
  store i64 %length, i64* %length.addr, align 8
  %call = call i8* @malloc(i64 16) #4
  %0 = bitcast i8* %call to %struct.String*
  store %struct.String* %0, %struct.String** %strPtr, align 8
  %1 = load i64, i64* %length.addr, align 8
  %call1 = call i8* @malloc(i64 %1) #4
  store i8* %call1, i8** %buffer, align 8
  %2 = load i64, i64* %length.addr, align 8
  %3 = load %struct.String*, %struct.String** %strPtr, align 8
  %string_length = getelementptr inbounds %struct.String, %struct.String* %3, i32 0, i32 0
  store i64 %2, i64* %string_length, align 8
  %4 = load i8*, i8** %buffer, align 8
  %5 = load %struct.String*, %struct.String** %strPtr, align 8
  %data = getelementptr inbounds %struct.String, %struct.String* %5, i32 0, i32 1
  store i8* %4, i8** %data, align 8
  %6 = load %struct.String*, %struct.String** %strPtr, align 8
  ret %struct.String* %6
}

; Function Attrs: allocsize(0)
declare i8* @malloc(i64) #3

; Function Attrs: noinline nounwind optnone ssp uwtable
define %struct.String* @plusStr(%struct.String* %a, %struct.String* %b) #0 {
entry:
  %a.addr = alloca %struct.String*, align 8
  %b.addr = alloca %struct.String*, align 8
  %num_chars = alloca i32, align 4
  %str = alloca %struct.String*, align 8
  store %struct.String* %a, %struct.String** %a.addr, align 8
  store %struct.String* %b, %struct.String** %b.addr, align 8
  %0 = load %struct.String*, %struct.String** %a.addr, align 8
  %string_length = getelementptr inbounds %struct.String, %struct.String* %0, i32 0, i32 0
  %1 = load i64, i64* %string_length, align 8
  %2 = load %struct.String*, %struct.String** %b.addr, align 8
  %string_length1 = getelementptr inbounds %struct.String, %struct.String* %2, i32 0, i32 0
  %3 = load i64, i64* %string_length1, align 8
  %add = add i64 %1, %3
  %conv = trunc i64 %add to i32
  store i32 %conv, i32* %num_chars, align 4
  %4 = load i32, i32* %num_chars, align 4
  %conv2 = sext i32 %4 to i64
  %call = call %struct.String* @mkString(i64 %conv2)
  store %struct.String* %call, %struct.String** %str, align 8
  %5 = load %struct.String*, %struct.String** %str, align 8
  %data = getelementptr inbounds %struct.String, %struct.String* %5, i32 0, i32 1
  %6 = load i8*, i8** %data, align 8
  %7 = load i32, i32* %num_chars, align 4
  %conv3 = sext i32 %7 to i64
  %8 = load %struct.String*, %struct.String** %str, align 8
  %data4 = getelementptr inbounds %struct.String, %struct.String* %8, i32 0, i32 1
  %9 = load i8*, i8** %data4, align 8
  %10 = call i64 @llvm.objectsize.i64.p0i8(i8* %9, i1 false, i1 true)
  %11 = load %struct.String*, %struct.String** %a.addr, align 8
  %data5 = getelementptr inbounds %struct.String, %struct.String* %11, i32 0, i32 1
  %12 = load i8*, i8** %data5, align 8
  %13 = load %struct.String*, %struct.String** %b.addr, align 8
  %data6 = getelementptr inbounds %struct.String, %struct.String* %13, i32 0, i32 1
  %14 = load i8*, i8** %data6, align 8
  %call7 = call i32 (i8*, i64, i32, i64, i8*, ...) @__snprintf_chk(i8* %6, i64 %conv3, i32 0, i64 %10, i8* getelementptr inbounds ([5 x i8], [5 x i8]* @.str.1, i32 0, i32 0), i8* %12, i8* %14)
  %15 = load %struct.String*, %struct.String** %str, align 8
  ret %struct.String* %15
}

; Function Attrs: noinline nounwind optnone ssp uwtable
define i32 @main() #0 {
entry:
  %retval = alloca i32, align 4
  %returnVal = alloca %struct.String*, align 8
  store i32 0, i32* %retval, align 4
  %call = call %struct.String* (...) @module_main()
  store %struct.String* %call, %struct.String** %returnVal, align 8
  %0 = load %struct.String*, %struct.String** %returnVal, align 8
  %data = getelementptr inbounds %struct.String, %struct.String* %0, i32 0, i32 1
  %1 = load i8*, i8** %data, align 8
  %call1 = call i32 (i8*, ...) @printf(i8* getelementptr inbounds ([4 x i8], [4 x i8]* @.str.2, i32 0, i32 0), i8* %1)
  ret i32 0
}

declare %struct.String* @module_main(...) #1

declare i32 @printf(i8*, ...) #1

attributes #0 = { noinline nounwind optnone ssp uwtable "correctly-rounded-divide-sqrt-fp-math"="false" "disable-tail-calls"="false" "less-precise-fpmad"="false" "no-frame-pointer-elim"="true" "no-frame-pointer-elim-non-leaf" "no-infs-fp-math"="false" "no-jump-tables"="false" "no-nans-fp-math"="false" "no-signed-zeros-fp-math"="false" "no-trapping-math"="false" "stack-protector-buffer-size"="8" "target-cpu"="penryn" "target-features"="+cx16,+fxsr,+mmx,+sse,+sse2,+sse3,+sse4.1,+ssse3,+x87" "unsafe-fp-math"="false" "use-soft-float"="false" }
attributes #1 = { "correctly-rounded-divide-sqrt-fp-math"="false" "disable-tail-calls"="false" "less-precise-fpmad"="false" "no-frame-pointer-elim"="true" "no-frame-pointer-elim-non-leaf" "no-infs-fp-math"="false" "no-nans-fp-math"="false" "no-signed-zeros-fp-math"="false" "no-trapping-math"="false" "stack-protector-buffer-size"="8" "target-cpu"="penryn" "target-features"="+cx16,+fxsr,+mmx,+sse,+sse2,+sse3,+sse4.1,+ssse3,+x87" "unsafe-fp-math"="false" "use-soft-float"="false" }
attributes #2 = { nounwind readnone speculatable }
attributes #3 = { allocsize(0) "correctly-rounded-divide-sqrt-fp-math"="false" "disable-tail-calls"="false" "less-precise-fpmad"="false" "no-frame-pointer-elim"="true" "no-frame-pointer-elim-non-leaf" "no-infs-fp-math"="false" "no-nans-fp-math"="false" "no-signed-zeros-fp-math"="false" "no-trapping-math"="false" "stack-protector-buffer-size"="8" "target-cpu"="penryn" "target-features"="+cx16,+fxsr,+mmx,+sse,+sse2,+sse3,+sse4.1,+ssse3,+x87" "unsafe-fp-math"="false" "use-soft-float"="false" }
attributes #4 = { allocsize(0) }

!llvm.module.flags = !{!0, !1}
!llvm.ident = !{!2}

!0 = !{i32 1, !"wchar_size", i32 4}
!1 = !{i32 7, !"PIC Level", i32 2}
!2 = !{!"clang version 5.0.0 (tags/RELEASE_500/final)"}
