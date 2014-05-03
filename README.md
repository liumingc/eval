一个简化的scheme 解释器/编译器。

#### 数据类型

- boolean，`#t`, `#f`
- number
- string
- symbol
- function，包括基本过程、复合过程
- list/pair，空表`()`也归入此列


#### 语法形式

- def
- set!
- if
- fn  
lambda 写起来太麻烦了，使用fn。
- quote
- quasiquote, unquote, splice-unquote
- begin
- call/cc
- defmacro

#### 性能优化

暂时不是考虑的重点。

#### 参考书目

- SICP  
抽象屏障当然重要，但是感觉书中的例子，也太啰嗦了，可读性也没有太大的提高。
不如dybvig使用record、record-case写的简洁、易读。
- 编译器原理
- arc的源码
