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
- quote
- quasiquote, unquote, splice-unquote
- begin
- call/cc

#### 性能优化
暂时不是考虑的重点。

#### 参考书目
- SICP
- 编译器原理
- arc的源码
