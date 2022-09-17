use std::{
    fmt::{Debug, Display},
    fs::{self, DirEntry},
    io::Error,
    io::Write,
    path::Path,
};

use crate::{
    asm::{self, ASMLine, ASMParsed, ASMUnit, Jump},
    common::{err_invalid_input, DirUnit, FileUnit, Unit, UnitFactory},
    parse::*,
    symbol::Symbol,
};

#[derive(Clone)]
pub struct VMUnit {
    src_path: String,
}

impl Unit for VMUnit {
    type Syntax = VMParsed;

    fn src_path(&self) -> &str {
        self.src_path.as_str()
    }
}

impl FileUnit for VMUnit {
    fn parse(&self) -> Result<Self::Syntax, Error> {
        VMParsed::from_unit(self)
    }
}

impl VMUnit {
    fn src_ext() -> &'static str {
        ".vm"
    }

    pub fn new(src_path: String) -> Self
    where
        Self: Sized,
    {
        Self { src_path }
    }

    fn out_unit(&self) -> ASMUnit {
        ASMUnit::new(
            self.src_path()
                .to_owned()
                .replace(Self::src_ext(), ASMUnit::src_ext()),
        )
    }
}

pub struct VMParsed {
    basename: String,
    source: Vec<VMLine>,
}

impl<'u> VMParsed {
    fn from_unit(unit: &'u VMUnit) -> Result<Self, Error> {
        let src_path: &Path = Path::new(unit.src_path());
        let basename = src_path
            .file_prefix()
            .and_then(|name| name.to_str().to_owned())
            .to_owned()
            .unwrap_or("Unit")
            .to_owned();

        let source = std::fs::read_to_string(src_path)?;
        Self::parse_new(basename, source)
    }

    pub fn new(basename: String, source: Vec<VMLine>) -> Self {
        Self { basename, source }
    }

    fn parse_new(basename: String, source: String) -> Result<Self, Error> {
        let parser = VMParser::new();
        match parser
            .parse(source.as_str())
            .map(|(_rem, lines)| VMParsed::new(basename, lines))
        {
            Ok(parsed) => Ok(parsed),
            Err(error_at) => Err(err_invalid_input(format!("parse error at: {}", error_at))),
        }
    }

    fn static_basename(&self) -> &str {
        &self.basename
    }

    fn lines(&self) -> Vec<&VMLine> {
        self.source.iter().collect()
    }
}

impl Display for VMParsed {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        for line in self.lines().iter().filter(|line| line.is_sloc()) {
            writeln!(f, "{}", line)?
        }
        Ok(())
    }
}

impl Debug for VMParsed {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        for line in self.lines().iter().filter(|line| line.is_sloc()) {
            writeln!(f, "{}", line)?
        }
        Ok(())
    }
}

struct SubCounter {
    eq: usize,
    gt: usize,
    lt: usize,
    call: usize,
    has_ret: bool,
    init: bool,
    fname: Option<String>,
    fcall: usize,
    has_call: bool,
}

impl SubCounter {
    fn new() -> Self {
        Self {
            eq: 0,
            gt: 0,
            lt: 0,
            call: 0,
            has_ret: false,
            init: false,
            fname: None,
            fcall: 0,
            has_call: false,
        }
    }

    fn inc_eq(&self) -> Self {
        Self {
            eq: self.eq + 1,
            gt: self.gt,
            lt: self.lt,
            call: self.call,
            has_ret: self.has_ret,
            init: self.init,
            fname: self.fname.as_ref().map(String::from),
            fcall: self.fcall,
            has_call: self.has_call,
        }
    }

    fn inc_gt(&self) -> Self {
        Self {
            eq: self.eq,
            gt: self.gt + 1,
            lt: self.lt,
            call: self.call,
            has_ret: self.has_ret,
            init: self.init,
            fname: self.fname.as_ref().map(String::from),
            fcall: self.fcall,
            has_call: self.has_call,
        }
    }

    fn inc_lt(&self) -> Self {
        Self {
            eq: self.eq,
            gt: self.gt,
            lt: self.lt + 1,
            call: self.call,
            has_ret: self.has_ret,
            init: self.init,
            fname: self.fname.as_ref().map(String::from),
            fcall: self.fcall,
            has_call: self.has_call,
        }
    }

    fn inc_call(&self) -> Self {
        Self {
            eq: self.eq,
            gt: self.gt,
            lt: self.lt,
            call: if self.fname.is_none() {
                self.call + 1
            } else {
                self.call
            },
            has_ret: self.has_ret,
            init: self.init,
            fname: self.fname.as_ref().map(String::from),
            fcall: if self.fname.is_some() {
                self.fcall + 1
            } else {
                self.fcall
            },
            has_call: true,
        }
    }

    fn push_fname(&self, fname: &str) -> Self {
        Self {
            eq: self.eq,
            gt: self.gt,
            lt: self.lt,
            call: self.call,
            has_ret: self.has_ret,
            init: self.init,
            fname: Some(fname.to_owned()),
            fcall: 0,
            has_call: self.has_call,
        }
    }

    fn inc_ret(&self) -> Self {
        Self {
            eq: self.eq,
            gt: self.gt,
            lt: self.lt,
            call: self.call,
            has_ret: true,
            init: self.init,
            fname: self.fname.as_ref().map(String::from),
            fcall: 0,
            has_call: self.has_call,
        }
    }

    fn with_sys_init(&self) -> Self {
        Self {
            eq: self.eq,
            gt: self.gt,
            lt: self.lt,
            call: self.call,
            has_ret: self.has_ret,
            init: true,
            fname: self.fname.as_ref().map(String::from),
            fcall: self.fcall,
            has_call: self.has_call,
        }
    }

    fn current_fname(&self) -> Option<&str> {
        self.fname.as_deref()
    }

    fn current_call(&self) -> usize {
        if self.fname.is_some() {
            self.fcall
        } else {
            self.call
        }
    }

    fn has_any_subs(&self) -> bool {
        self.eq > 0 || self.gt > 0 || self.lt > 0 || self.has_ret || self.has_call
    }

    fn combine(&self, other: &Self) -> Self {
        Self {
            eq: self.eq + other.eq,
            gt: self.gt + other.gt,
            lt: self.lt + other.lt,
            call: self.call + other.call,
            has_ret: self.has_ret || other.has_ret,
            init: self.init || other.init,
            fname: None,
            fcall: 0,
            has_call: self.has_call || other.has_call,
        }
    }
}

pub struct CodeWriter {
    parsed: VMParsed,
    lines: Vec<ASMLine>,
    counter: SubCounter,
}

const MICRO_SP_ADVANCE: &str = r#"  @SP
                                    AM=M+1"#;

const MICRO_PUSH_D: &str = r#"  @SP
                                AM=M+1
                                A=A-1
                                M=D"#;

const MICRO_POP_D: &str = r#"   @SP
                                AM=M-1
                                D=M"#;

impl CodeWriter {
    fn new(parsed: VMParsed) -> Self {
        let (lines, counter): (Vec<_>, SubCounter) = parsed
            .lines()
            .iter()
            .filter(|line| line.is_sloc())
            .fold((vec![], SubCounter::new()), |(acc, counter), line| {
                let (transformed, new_counter) =
                    Self::translate_line(parsed.static_basename(), line, counter);
                (vec![acc, transformed].concat(), new_counter)
            });
        Self {
            parsed,
            lines,
            counter,
        }
    }

    fn push_from_segment(
        segment: &str,
        counter: SubCounter,
        index: &i16,
    ) -> (Vec<ASMLine>, SubCounter) {
        (
            asm::now(
                format!(
                    r#"
                    @{index}
                    D=A
                    @{segment}
                    A=D+M
                    D=M
                    {MICRO_PUSH_D}
                    "#
                )
                .as_str(),
            ),
            counter,
        )
    }

    fn pop_to_segment(
        segment: &str,
        counter: SubCounter,
        index: &i16,
    ) -> (Vec<ASMLine>, SubCounter) {
        (
            asm::now(
                format!(
                    r#"
                    @{index}
                    D=A
                    @{segment}
                    D=D+M
                    @SP
                    A=M
                    M=D
                    {MICRO_POP_D}
                    @SP
                    A=M
                    A=A+1
                    A=M
                    M=D
                    "#
                )
                .as_str(),
            ),
            counter,
        )
    }

    fn call_cmp_subroutine(
        cmp_op: &str,
        counter: &SubCounter,
        count: &usize,
        fn_counter: fn(counter: &SubCounter) -> SubCounter,
    ) -> (Vec<ASMLine>, SubCounter) {
        (
            asm::now(
                format!(
                    r#"
                    @RET_ADDRESS_{cmp_op}{count}
                    D=A
                    @BEGIN_{cmp_op}
                    0;JMP
                    (RET_ADDRESS_{cmp_op}{count})
                    "#,
                )
                .as_str(),
            ),
            fn_counter(counter),
        )
    }

    fn call_unary_arithmetic(math_op: &str, counter: SubCounter) -> (Vec<ASMLine>, SubCounter) {
        (
            asm::now(
                format!(
                    r#"
                    @SP
                    AM=M-1
                    M={math_op}
                    @SP
                    M=M+1
                    "#
                )
                .as_str(),
            ),
            counter,
        )
    }

    fn call_binary_arithmetic(math_op: &str, counter: SubCounter) -> (Vec<ASMLine>, SubCounter) {
        (
            asm::now(
                format!(
                    r#"
                    {MICRO_POP_D}
                    @SP
                    AM=M-1
                    M={math_op}
                    @SP
                    M=M+1
                    "#
                )
                .as_str(),
            ),
            counter,
        )
    }

    fn translate_line(
        basename: &str,
        line: &VMLine,
        counter: SubCounter,
    ) -> (Vec<ASMLine>, SubCounter) {
        let label_prefix = counter.current_fname().unwrap_or(basename);
        let result = match line {
            VMLine::Push(Segment::Constant, index) => (
                asm::now(
                    format!(
                        r#"
                        @{index}
                        D=A
                        {MICRO_PUSH_D}
                        "#
                    )
                    .as_str(),
                ),
                counter,
            ),
            VMLine::Pop(Segment::Constant, _) => panic!("can't pop to a constant!"),
            VMLine::Push(Segment::Pointer, index) => {
                let register = if index == &0 {
                    "THIS"
                } else if index == &1 {
                    "THAT"
                } else {
                    panic!("illegal index for pointer access: {}", index)
                };
                (
                    asm::now(
                        format!(
                            r#"
                            @{register}
                            D=M
                            {MICRO_PUSH_D}
                            "#
                        )
                        .as_str(),
                    ),
                    counter,
                )
            }
            VMLine::Pop(Segment::Pointer, index) => {
                let register = if index == &0 {
                    "THIS"
                } else if index == &1 {
                    "THAT"
                } else {
                    panic!("illegal index for pointer access: {}", index)
                };
                (
                    asm::now(
                        format!(
                            r#"
                            {MICRO_POP_D}
                            @{register}
                            M=D
                            "#
                        )
                        .as_str(),
                    ),
                    counter,
                )
            }
            VMLine::Push(Segment::Temp, index) => {
                if index < &0 || index > &10 {
                    panic!("illegal index for temp access: {}", index);
                };
                let reg_idx = index + 5;
                (
                    asm::now(
                        format!(
                            r#"
                            @{reg_idx}
                            D=A
                            @TEMP
                            A=D+M
                            D=M
                            {MICRO_PUSH_D}
                            "#
                        )
                        .as_str(),
                    ),
                    counter,
                )
            }
            VMLine::Pop(Segment::Temp, index) => {
                if index < &0 || index > &10 {
                    panic!("illegal index for temp access: {}", index);
                };
                let reg_idx = index + 5;
                (
                    asm::now(
                        format!(
                            r#"
                            {MICRO_POP_D}
                            @R{reg_idx}
                            M=D
                            "#
                        )
                        .as_str(),
                    ),
                    counter,
                )
            }
            VMLine::Push(Segment::Local, index) => Self::push_from_segment("LCL", counter, index),
            VMLine::Pop(Segment::Local, index) => Self::pop_to_segment("LCL", counter, index),
            VMLine::Push(Segment::Argument, index) => {
                Self::push_from_segment("ARG", counter, index)
            }
            VMLine::Pop(Segment::Argument, index) => Self::pop_to_segment("ARG", counter, index),
            VMLine::Push(Segment::This, index) => Self::push_from_segment("THIS", counter, index),
            VMLine::Pop(Segment::This, index) => Self::pop_to_segment("THIS", counter, index),
            VMLine::Push(Segment::That, index) => Self::push_from_segment("THAT", counter, index),
            VMLine::Pop(Segment::That, index) => Self::pop_to_segment("THAT", counter, index),
            VMLine::Push(Segment::Static, index) => (
                asm::now(
                    format!(
                        r#"
                        @{basename}.{index}
                        D=M
                        {MICRO_PUSH_D}
                        "#
                    )
                    .as_str(),
                ),
                counter,
            ),
            VMLine::Pop(Segment::Static, index) => (
                asm::now(
                    format!(
                        r#"
                        {MICRO_POP_D}
                        @{basename}.{index}
                        M=D
                        "#
                    )
                    .as_str(),
                ),
                counter,
            ),
            VMLine::Add => Self::call_binary_arithmetic("D+M", counter),
            VMLine::Sub => Self::call_binary_arithmetic("M-D", counter),
            VMLine::Neg => Self::call_unary_arithmetic("-M", counter),
            VMLine::And => Self::call_binary_arithmetic("D&M", counter),
            VMLine::Or => Self::call_binary_arithmetic("D|M", counter),
            VMLine::Not => Self::call_unary_arithmetic("!M", counter),
            VMLine::Eq => {
                Self::call_cmp_subroutine("EQ", &counter, &counter.eq, |cntr| cntr.inc_eq())
            }
            VMLine::Gt => {
                Self::call_cmp_subroutine("GT", &counter, &counter.gt, |cntr| cntr.inc_gt())
            }
            VMLine::Lt => {
                Self::call_cmp_subroutine("LT", &counter, &counter.lt, |cntr| cntr.inc_lt())
            }
            VMLine::Label(sym) => (
                vec![ASMLine::LInstr(
                    [label_prefix, sym.as_str()].join("$").as_str().into(),
                )],
                counter,
            ),
            VMLine::Goto(sym) => (
                asm::now(
                    format!(
                        r#"
                        @{label_prefix}${sym}
                        0;JMP
                        "#
                    )
                    .as_str(),
                ),
                counter,
            ),
            VMLine::IfGoto(sym) => (
                asm::now(
                    format!(
                        r#"
                        {MICRO_POP_D}
                        @{label_prefix}${sym}
                        D;JNE
                        "#
                    )
                    .as_str(),
                ),
                counter,
            ),
            VMLine::Function(name, n_vars) => {
                let fnname = if !name.contains('.') {
                    [basename, name.as_str()].join(".")
                } else {
                    name.as_str().to_owned()
                };
                let pushes: Vec<_> = (0..*n_vars).map(|_| MICRO_PUSH_D).collect();
                let lvars_init = pushes.join("\n");
                (
                    asm::now(
                        format!(
                            r#"
                            ({fnname})
                            D=0
                            {lvars_init}
                            "#
                        )
                        .as_str(),
                    ),
                    if "Sys.init" == fnname {
                        counter.with_sys_init()
                    } else {
                        counter
                    }
                    .push_fname(fnname.as_str()),
                )
            }
            VMLine::Return => (
                asm::now(
                    format!(
                        r#"
                        @BEGIN_RETURN
                        0;JMP
                        "#
                    )
                    .as_str(),
                ),
                counter.inc_ret(),
            ),
            VMLine::Call(name, n_args) => {
                let fnname = if !name.contains('.') {
                    [basename, name.as_str()].join(".")
                } else {
                    name.as_str().to_owned()
                };
                (
                    asm::now(
                        format!(
                            r#"
                            @{n_args}
                            D=A
                            @R13
                            M=D
                            @{fnname}
                            D=A
                            @R14
                            M=D
                            @{label_prefix}$ret{count}
                            D=A
                            @BEGIN_CALL
                            0;JMP
                            ({label_prefix}$ret{count})
                            "#,
                            count = counter.current_call()
                        )
                        .as_str(),
                    ),
                    counter.inc_call(),
                )
            }
            _ => (
                vec![ASMLine::Comment("unhandled line!!!".to_owned())],
                counter,
            ),
        };
        let comment = vec![ASMLine::Comment(line.to_string())];
        if COMMENT_ALL_LINES {
            let (lines, counter) = result;
            (vec![comment, lines].concat(), counter)
        } else {
            result
        }
    }

    fn lines<'a>(&'a self) -> Vec<&'a ASMLine> {
        self.lines.iter().map(|line| line).collect()
    }

    fn into_asm(self) -> Vec<ASMLine> {
        self.lines
    }
}

const COMMENT_ALL_LINES: bool = true;
impl Display for CodeWriter {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        for line in self.lines() {
            writeln!(f, "{}", line)?
        }
        Ok(())
    }
}

impl Debug for CodeWriter {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        for line in self.parsed.lines() {
            if line.is_sloc() {
                writeln!(f, "{}", line)?;
            }
        }
        Ok(())
    }
}

pub struct Bootstrapper {
    counter: SubCounter,
    writers: Vec<CodeWriter>,
}

impl Bootstrapper {
    fn new(parseds: Vec<VMParsed>) -> Self {
        let writers: Vec<_> = parseds.into_iter().map(|p| CodeWriter::new(p)).collect();
        let counter = writers
            .iter()
            .fold(SubCounter::new(), |acc, w| acc.combine(&w.counter));
        Self { counter, writers }
    }

    fn prefix(&self) -> Vec<ASMLine> {
        vec![
            asm::now(
                r#"
                @SP
                D=M
                @END_SP_INIT
                D;JNE
                @256
                D=A
                @SP
                M=D
                (END_SP_INIT)
                "#,
            ),
            if self.counter.has_any_subs() {
                asm::now(
                    r#"  
                    @ENTRYPOINT
                    0;JMP
                    "#,
                )
            } else {
                vec![]
            },
            if self.counter.eq > 0 {
                self.init_cmp("BEGIN_EQ", "END_EQ", Jump::Jne)
            } else {
                Vec::new()
            },
            if self.counter.gt > 0 {
                self.init_cmp("BEGIN_GT", "END_GT", Jump::Jle)
            } else {
                Vec::new()
            },
            if self.counter.lt > 0 {
                self.init_cmp("BEGIN_LT", "END_LT", Jump::Jge)
            } else {
                Vec::new()
            },
            if self.counter.has_ret {
                self.init_sub_return()
            } else {
                Vec::new()
            },
            if self.counter.has_call {
                self.init_sub_call()
            } else {
                Vec::new()
            },
            if self.counter.has_any_subs() {
                asm::now(
                    r#"
                    (ENTRYPOINT)
                    "#,
                )
            } else {
                Vec::new()
            },
            if self.counter.init {
                asm::now(
                    r#"
                    @Sys.init
                    0;JMP
                    "#,
                )
            } else {
                Vec::new()
            },
        ]
        .concat()
    }

    fn init_cmp(&self, l_begin: &str, l_end: &str, j_false: Jump) -> Vec<ASMLine> {
        asm::now(
            format!(
                r#"
                ({l_begin})
                @R15
                M=D
                {MICRO_POP_D}
                A=A-1
                D=M-D
                M=0
                @{l_end}
                D;{j_false}
                @SP
                A=M-1
                M=-1
                ({l_end})
                @R15
                A=M
                0;JMP
                "#
            )
            .as_str(),
        )
    }

    fn init_sub_return(&self) -> Vec<ASMLine> {
        asm::now(
            format!(
                r#"
                (BEGIN_RETURN)
                @5
                D=A
                @LCL
                A=M-D
                D=M
                @R13 // R13 <- return address
                M=D
                {MICRO_POP_D}
                @ARG
                A=M
                M=D  // ARG base <- D
                D=A  // D <- @ARG
                @SP
                M=D+1 // @SP <- @ARG + 1 (ready to pop value for caller)
                @LCL  
                D=M    // D <- @LCL base address
                @R14   // @R14 becomes adhoc @SP
                AM=D-1 // @R14 <- retreat 1 to saved @THAT
                D=M    // D <- saved @THAT
                @THAT  
                M=D    // @THAT <- saved @THAT
                @R14   
                AM=M-1 // @R14 <- retreat 1 to saved @THIS
                D=M
                @THIS
                M=D    // @THIS <- saved @THIS
                @R14
                AM=M-1 // @R14 <- retreat 1 to saved @ARG
                D=M
                @ARG
                M=D    // @ARG <- saved @ARG
                @R14
                AM=M-1 // @R14 <- retreat 1 to saved @LCL
                D=M
                @LCL
                M=D    // @LCL <- saved @LCL
                @R13   
                A=M    // A <- R13
                0;JMP
                "#
            )
            .as_str(),
        )
    }

    fn init_sub_call(&self) -> Vec<ASMLine> {
        asm::now(
            format!(
                r#"
                (BEGIN_CALL)
                @SP
                A=M
                M=D
                @LCL
                D=M
                {MICRO_SP_ADVANCE}
                M=D
                @ARG
                D=M
                {MICRO_SP_ADVANCE}
                M=D
                @THIS
                D=M
                {MICRO_SP_ADVANCE}
                M=D
                @THAT
                D=M
                {MICRO_SP_ADVANCE}
                M=D
                @4      // SP-1-4
                D=A
                @R13
                D=D+M
                @SP
                D=M-D
                @ARG
                M=D
                @SP
                MD=M+1
                @LCL
                M=D // @LCL=@SP
                @R14
                A=M
                0;JMP
                "#
            )
            .as_str(),
        )
    }

    fn suffix(&self) -> Vec<ASMLine> {
        //(INFINITE_LOOP)
        //@INFINITE_LOOP
        //0;JMP
        asm::now(
            r#"
            (INFINITE_LOOP)
            @INFINITE_LOOP
            0;JMP
            "#,
        )
    }

    pub fn into_asm(self) -> Vec<ASMLine> {
        let prefix = (&self).prefix();
        let suffix = (&self).suffix();
        let from_writers: Vec<_> = self.writers.into_iter().map(|w| w.into_asm()).collect();
        vec![prefix, from_writers.concat(), suffix].concat()
    }
}

impl Display for Bootstrapper {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        for line in self.prefix().iter().filter(|line| !line.is_blank()) {
            writeln!(f, "{line}")?;
        }
        for writer in self.writers.iter() {
            for line in writer.lines().iter().filter(|line| !line.is_blank()) {
                writeln!(f, "{line}")?;
            }
        }
        for line in self.suffix().iter().filter(|line| !line.is_blank()) {
            writeln!(f, "{line}")?;
        }
        Ok(())
    }
}

impl Debug for Bootstrapper {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let mut line_num = 0;
        let width = format!(
            "{}",
            self.writers
                .iter()
                .fold(self.prefix().len() + self.suffix().len(), |total, w| total
                    + w.lines().iter().len())
        )
        .len();
        let line_width = 50;
        for line in self.prefix().iter().filter(|line| !line.is_blank()) {
            let line_str = format!("{line}");
            if line.is_indexed() {
                writeln!(f, "{line_str:line_width$} //   {line_num:width$}")?;
                line_num = line_num + 1;
            } else {
                writeln!(f, "{line:line_width$}")?;
            }
        }
        for writer in self.writers.iter() {
            for line in writer.lines().iter().filter(|line| !line.is_blank()) {
                let line_str = format!("{line}");
                if line.is_indexed() {
                    writeln!(f, "{line_str:line_width$} //   {line_num:width$}")?;
                    line_num = line_num + 1;
                } else {
                    writeln!(f, "{line:line_width$}")?;
                }
            }
        }
        for line in self.suffix().iter().filter(|line| !line.is_blank()) {
            let line_str = format!("{line}");
            if line.is_indexed() {
                writeln!(f, "{line_str:line_width$} //   {line_num:width$}")?;
                line_num = line_num + 1;
            } else {
                writeln!(f, "{line:line_width$}")?;
            }
        }
        Ok(())
    }
}

pub enum VMUnitType {
    File(VMUnit),
    Dir(VMDirUnit),
}

impl Unit for VMUnitType {
    type Syntax = VMParsed;

    fn src_path<'a>(&'a self) -> &'a str {
        match self {
            VMUnitType::File(unit) => unit.src_path(),
            VMUnitType::Dir(unit) => unit.src_path(),
        }
    }
}

impl DirUnit for VMUnitType {
    fn filename_for(elem: &VMParsed) -> String {
        format!("{}.vm", elem.static_basename())
    }

    fn parse(&self) -> Result<Vec<Self::Syntax>, Error> {
        match self {
            VMUnitType::File(unit) => unit.parse().map(|parsed| vec![parsed]),
            VMUnitType::Dir(unit) => unit.parse(),
        }
    }
}

impl VMUnitType {
    fn out_unit(&self) -> ASMUnit {
        match self {
            VMUnitType::File(unit) => unit.out_unit(),
            VMUnitType::Dir(unit) => unit.out_unit(),
        }
    }
}

pub struct VMUnitFactory {}

impl VMUnitFactory {
    fn read_vm(src_path: &str) -> Result<VMUnitType, Error> {
        if !src_path.ends_with(".vm") {
            return Err(err_invalid_input(format!("Invalid filename: {}", src_path)));
        }
        Ok(VMUnitType::File(VMUnit::new(src_path.to_owned())))
    }

    pub fn read_dir_as_dir(src_path: &str) -> Result<VMDirUnit, Error> {
        let out_name = Path::new(src_path)
            .file_name()
            .unwrap()
            .to_owned()
            .into_string()
            .expect("failed to convert OsString to String");
        let out_path = format!("{src_path}/{out_name}.asm");
        Ok(VMDirUnit::new(src_path.to_owned(), out_path))
    }

    pub fn read_dir(src_path: &str) -> Result<VMUnitType, Error> {
        Self::read_dir_as_dir(src_path).map(|unit| VMUnitType::Dir(unit))
    }
}

impl UnitFactory for VMUnitFactory {
    type Unit = VMUnitType;
    fn unit_from(src_path: &str) -> Result<Self::Unit, Error> {
        let path_as_path = Path::new(src_path);
        if path_as_path.is_dir() {
            // for fs::read_dir(path).ok()
            Self::read_dir(src_path) // .expect(&format!("failed to read directory {}", src_path))
        } else if src_path.ends_with(".vm") {
            Self::read_vm(src_path) // .expect(&format!("failed to read vm {}", src_path))
        } else {
            Err(err_invalid_input(format!(
                "invalid source file extension {}",
                src_path
            )))
        }
    }
}

pub struct VMDirUnit {
    src_path: String,
    out_path: String,
}

impl Unit for VMDirUnit {
    type Syntax = VMParsed;

    fn src_path<'a>(&'a self) -> &'a str {
        &self.src_path
    }
}

impl DirUnit for VMDirUnit {
    fn filename_for(elem: &VMParsed) -> String {
        let mut base_path = elem.static_basename().to_owned();
        base_path.push_str(".vm");
        base_path
    }

    fn parse(&self) -> Result<Vec<Self::Syntax>, Error> {
        DirParser::parse_all(self)
    }
}

impl VMDirUnit {
    fn new(src_path: String, out_path: String) -> Self {
        Self { src_path, out_path }
    }
    fn out_unit(&self) -> ASMUnit {
        ASMUnit::new(self.out_path.as_str().to_owned())
    }
}

pub struct DirParser {}

impl DirParser {
    fn parse_all(unit: &VMDirUnit) -> Result<Vec<VMParsed>, Error> {
        let children: Result<Vec<DirEntry>, Error> = fs::read_dir(unit.src_path())?.collect();
        let vm_files: Vec<String> = children?
            .iter()
            .filter_map(|child| child.path().to_str().map(|p| String::from(p)))
            .filter(|name| name.ends_with(".vm"))
            .collect();
        let units: Result<Vec<VMUnitType>, Error> = vm_files
            .iter()
            .map(|src_path| VMUnitFactory::read_vm(src_path))
            .collect();
        units?
            .iter()
            .map(|wrapped| match wrapped {
                VMUnitType::File(vm_unit) => VMParsed::from_unit(vm_unit),
                _ => Err(err_invalid_input("had a dir type in a dir type")),
            })
            .collect()
    }
}

pub struct VMParser {}

impl VMParser {
    pub fn new() -> Self {
        VMParser {}
    }
}

impl<'a> Parser<'a, &'a str, Vec<VMLine>> for VMParser {
    fn parse(&self, input: &'a str) -> ParseResult<'a, &'a str, Vec<VMLine>> {
        range(
            right(
                space0(),
                left(VMLine::parser(), right(pad0(), or_else(newline(), eof))),
            ),
            0..,
        )
        .parse(input)
    }
}

#[derive(Debug, Clone, Copy)]
pub enum Segment {
    Argument,
    Local,
    Static,
    Constant,
    This,
    That,
    Pointer,
    Temp,
    Invalid,
}

impl Segment {
    fn as_str(&self) -> &'static str {
        match self {
            &Self::Argument => "argument",
            &Self::Local => "local",
            &Self::Static => "static",
            &Self::Constant => "constant",
            &Self::This => "this",
            &Self::That => "that",
            &Self::Pointer => "pointer",
            &Self::Temp => "temp",
            &Self::Invalid => "!!!",
        }
    }

    fn is_invalid(&self) -> bool {
        match self {
            &Self::Invalid => true,
            _ => false,
        }
    }
}

impl From<&str> for Segment {
    fn from(item: &str) -> Self {
        match item {
            "argument" => Self::Argument,
            "local" => Self::Local,
            "static" => Self::Static,
            "constant" => Self::Constant,
            "this" => Self::This,
            "that" => Self::That,
            "pointer" => Self::Pointer,
            "temp" => Self::Temp,
            _ => Self::Invalid,
        }
    }
}

impl Display for Segment {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            &Self::Invalid => Err(std::fmt::Error),
            _ => write!(f, "{}", self.as_str()),
        }
    }
}

enum CommandType {
    Arithmetic,
    Push,
    Pop,
    Label,
    Goto,
    If,
    Function,
    Return,
    Call,
    Invalid,
}
#[derive(PartialEq, Eq, Clone, Copy, Debug)]
pub enum Command {
    Push,
    Pop,
    Add,
    Sub,
    Neg,
    Eq,
    Gt,
    Lt,
    And,
    Or,
    Not,
    Label,
    Goto,
    IfGoto,
    Function,
    Return,
    Call,
    Invalid,
}

impl Command {
    fn as_str(&self) -> &'static str {
        match self {
            &Self::Push => "push",
            &Self::Pop => "pop",
            &Self::Add => "add",
            &Self::Sub => "sub",
            &Self::Neg => "neg",
            &Self::Eq => "eq",
            &Self::Gt => "gt",
            &Self::Lt => "lt",
            &Self::And => "and",
            &Self::Or => "or",
            &Self::Not => "not",
            &Self::Label => "label",
            &Self::Goto => "goto",
            &Self::IfGoto => "if-goto",
            &Self::Function => "function",
            &Self::Return => "return",
            &Self::Call => "call",
            &Self::Invalid => "!!!",
        }
    }

    fn get_type(&self) -> CommandType {
        match self {
            &Self::Add
            | &Self::Sub
            | &Self::Neg
            | &Self::Eq
            | &Self::Gt
            | &Self::Lt
            | &Self::And
            | &Self::Or
            | &Self::Not => CommandType::Arithmetic,
            &Self::Push => CommandType::Push,
            &Self::Pop => CommandType::Pop,
            &Self::Label => CommandType::Label,
            &Self::Goto => CommandType::Goto,
            &Self::IfGoto => CommandType::If,
            &Self::Function => CommandType::Function,
            &Self::Return => CommandType::Return,
            &Self::Call => CommandType::Call,
            &Self::Invalid => CommandType::Invalid,
        }
    }

    fn is_arithmetic(&self) -> bool {
        match self.get_type() {
            CommandType::Arithmetic => true,
            _ => false,
        }
    }

    fn is_return(&self) -> bool {
        match self {
            &Self::Return => true,
            _ => false,
        }
    }

    fn segment_needed(&self) -> bool {
        match self {
            &Self::Push | &Self::Pop => true,
            _ => false,
        }
    }

    fn args_needed(&self) -> usize {
        if self.is_arithmetic() || self.is_return() {
            return 0;
        } else {
            match self {
                &Self::Push => 2,
                &Self::Pop => 2,
                &Self::Function => 2,
                &Self::Call => 2,
                _ => 1,
            }
        }
    }
    fn all() -> Vec<Self> {
        vec![
            Self::Push,
            Self::Pop,
            Self::Add,
            Self::Sub,
            Self::Neg,
            Self::Eq,
            Self::Gt,
            Self::Lt,
            Self::And,
            Self::Or,
            Self::Not,
            Self::Label,
            Self::Goto,
            Self::IfGoto,
            Self::Function,
            Self::Return,
            Self::Call,
        ]
    }
}

impl From<&str> for Command {
    fn from(item: &str) -> Self {
        match item {
            "push" => Self::Push,
            "pop" => Self::Pop,
            "add" => Self::Add,
            "sub" => Self::Sub,
            "neg" => Self::Neg,
            "eq" => Self::Eq,
            "gt" => Self::Gt,
            "lt" => Self::Lt,
            "and" => Self::And,
            "or" => Self::Or,
            "not" => Self::Not,
            "label" => Self::Label,
            "goto" => Self::Goto,
            "if-goto" => Self::IfGoto,
            "function" => Self::Function,
            "return" => Self::Return,
            "call" => Self::Call,
            _ => Self::Invalid,
        }
    }
}

impl Display for Command {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.as_str())
    }
}

#[derive(Debug)]
pub enum VMLine {
    Comment(String),
    Push(Segment, i16),
    Pop(Segment, i16),
    Function(String, i16),
    Call(String, i16),
    Return,
    Label(String),
    Goto(String),
    IfGoto(String),
    Add,
    Sub,
    Neg,
    Eq,
    Gt,
    Lt,
    And,
    Or,
    Not,
}

impl Clone for VMLine {
    fn clone(&self) -> Self {
        match self {
            Self::Comment(text) => Self::Comment(text.to_owned()),
            Self::Push(segment, index) => Self::Push(*segment, *index),
            Self::Pop(segment, index) => Self::Pop(*segment, *index),
            Self::Function(name, n_vars) => Self::Function(name.to_owned(), *n_vars),
            Self::Call(name, n_args) => Self::Call(name.to_owned(), *n_args),
            Self::Return => Self::Return,
            Self::Label(label) => Self::Label(label.to_owned()),
            Self::Goto(label) => Self::Goto(label.to_owned()),
            Self::IfGoto(label) => Self::IfGoto(label.to_owned()),
            Self::Add => Self::Add,
            Self::Sub => Self::Sub,
            Self::Neg => Self::Neg,
            Self::Eq => Self::Eq,
            Self::Gt => Self::Gt,
            Self::Lt => Self::Lt,
            Self::And => Self::And,
            Self::Or => Self::Or,
            Self::Not => Self::Not,
        }
    }
}

impl VMLine {
    fn from_0arg(command: &Command) -> Result<Self, Error> {
        match command {
            Command::Return => Ok(Self::Return),
            Command::Add => Ok(Self::Add),
            Command::Sub => Ok(Self::Sub),
            Command::Neg => Ok(Self::Neg),
            Command::And => Ok(Self::And),
            Command::Or => Ok(Self::Or),
            Command::Not => Ok(Self::Not),
            Command::Gt => Ok(Self::Gt),
            Command::Lt => Ok(Self::Lt),
            Command::Eq => Ok(Self::Eq),
            _ => Err(err_invalid_input(format!(
                "no args provided for command: {}",
                command
            ))),
        }
    }

    fn from_1arg(command: &Command, arg1: &Symbol) -> Result<Self, Error> {
        match command {
            Command::Label => Ok(Self::Label(arg1.to_string())),
            Command::Goto => Ok(Self::Goto(arg1.to_string())),
            Command::IfGoto => Ok(Self::IfGoto(arg1.to_string())),
            _ => Err(err_invalid_input(format!(
                "one arg provided for command: {}",
                command
            ))),
        }
    }

    fn from_2arg(command: &Command, arg1: &Symbol, arg2: i16) -> Result<Self, Error> {
        let segment: Segment = arg1.to_string().as_str().into();
        if command.segment_needed() && segment.is_invalid() {
            return Err(err_invalid_input(format!(
                "invalid segment for {} command: {}",
                command.as_str(),
                segment.as_str()
            )));
        }
        match command {
            Command::Push => Ok(Self::Push(segment, arg2)),
            Command::Pop => Ok(Self::Pop(segment, arg2)),
            Command::Function => Ok(Self::Function(arg1.to_string(), arg2)),
            Command::Call => Ok(Self::Call(arg1.to_string(), arg2)),
            _ => Err(err_invalid_input(format!(
                "two args provided for command: {}",
                command
            ))),
        }
    }

    fn get_command(&self) -> Command {
        match self {
            &Self::Push(..) => Command::Push,
            &Self::Pop(..) => Command::Pop,
            &Self::Function(..) => Command::Function,
            &Self::Call(..) => Command::Call,
            &Self::Label(..) => Command::Label,
            &Self::Goto(..) => Command::Goto,
            &Self::IfGoto(..) => Command::IfGoto,
            &Self::Return => Command::Return,
            &Self::Add => Command::Add,
            &Self::Sub => Command::Sub,
            &Self::Neg => Command::Neg,
            &Self::And => Command::And,
            &Self::Or => Command::Or,
            &Self::Not => Command::Not,
            &Self::Gt => Command::Gt,
            &Self::Lt => Command::Lt,
            &Self::Eq => Command::Eq,
            _ => Command::Invalid,
        }
    }

    fn is_sloc(&self) -> bool {
        match self {
            &Self::Comment(..) => false,
            _ => true,
        }
    }

    fn parser() -> VMLineParser {
        VMLineParser {}
    }
}

impl Display for VMLine {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Push(arg0, arg1) | Self::Pop(arg0, arg1) => {
                write!(f, "{} {} {}", self.get_command(), arg0, arg1)
            }
            Self::Function(arg0, arg1) | Self::Call(arg0, arg1) => {
                write!(f, "{} {} {}", self.get_command(), arg0, arg1)
            }
            Self::Label(arg0) | Self::Goto(arg0) | Self::IfGoto(arg0) => {
                write!(f, "{} {}", self.get_command(), arg0)
            }
            Self::Return
            | Self::Add
            | Self::Sub
            | Self::Neg
            | Self::And
            | Self::Or
            | Self::Not
            | Self::Gt
            | Self::Lt
            | Self::Eq => write!(f, "{}", self.get_command().as_str()),
            Self::Comment(text) => write!(f, "//{}", text),
        }
    }
}
pub struct VMLineParser {}

impl<'a> Parser<'a, &'a str, VMLine> for VMLineParser {
    fn parse(&self, input: &'a str) -> ParseResult<'a, &'a str, VMLine> {
        let init_parser: Box<dyn Parser<'a, &'a str, VMLine>> = Box::new(right(
            match_literal("//"),
            map(non_nl0(), |text| {
                VMLine::Comment(text.into_iter().collect())
            }),
        ));

        Command::all()
            .iter()
            .fold(init_parser, |acc, cmd| -> Box<dyn Parser<&str, VMLine>> {
                let trail_comment = right(pad0(), ok(right(match_literal("//"), non_nl0())));
                if cmd.args_needed() == 0 {
                    let parser = and_then(match_literal(cmd.as_str()), |()| VMLine::from_0arg(cmd));
                    return Box::new(or_else(
                        move |input| acc.parse(input),
                        left(parser, trail_comment),
                    ));
                } else if cmd.args_needed() == 1 {
                    let parser = and_then(
                        right(pair(match_literal(cmd.as_str()), pad1()), Symbol::parser()),
                        |arg1| VMLine::from_1arg(cmd, &arg1),
                    );
                    return Box::new(or_else(
                        move |input| acc.parse(input),
                        left(parser, trail_comment),
                    ));
                } else {
                    let parser = and_then(
                        right(
                            right(match_literal(cmd.as_str()), pad1()),
                            pair(left(Symbol::parser(), pad1()), i16_literal()),
                        ),
                        |(arg1, arg2)| VMLine::from_2arg(cmd, &arg1, arg2),
                    );
                    return Box::new(or_else(
                        move |input| acc.parse(input),
                        left(parser, trail_comment),
                    ));
                }
            })
            .parse(input)
    }
}

pub struct VMTranslator {}

impl VMTranslator {
    pub fn do_main(args: &[String]) -> Result<(), Error> {
        if args.is_empty() {
            Self::do_unit(".")?;
        } else {
            for arg in args {
                Self::do_unit(arg)?;
            }
        }
        Ok(())
    }

    fn do_unit(path: &str) -> Result<(), Error> {
        let unit = VMUnitFactory::unit_from(path).expect(&format!("failed to read unit {}", path));
        let code = unit.parse()?;
        writeln!(std::io::stdout(), "{:?}", code).ok();
        let asm_unit = unit.out_unit();
        let asm_syntax = ASMParsed::new(Bootstrapper::new(code).into_asm());
        asm_unit.save(&asm_syntax).expect("failed to save asm file");
        Ok(())
    }
}

#[cfg(test)]
mod tests {

    use crate::vm::{Bootstrapper, VMParsed};

    fn to_out(src: &str) -> String {
        let lines: Vec<_> = src
            .lines()
            .filter_map(|line| Some(line.trim()).filter(|l| !l.is_empty()))
            .collect();
        lines.join("\n") + "\n"
    }

    #[test]
    fn u07_stackarithmetic_simpleadd() {
        let source = r#"
            // This file is part of www.nand2tetris.org
            // and the book "The Elements of Computing Systems"
            // by Nisan and Schocken, MIT Press.
            // File name: projects/07/StackArithmetic/SimpleAdd/SimpleAdd.vm

            // Pushes and adds two constants.
            push constant 7
            push constant 8
            add
            "#
        .to_owned();

        let expect_parsed = to_out(
            r#"
            push constant 7
            push constant 8
            add
            "#,
        );

        let code = VMParsed::parse_new("".to_owned(), source).expect("expect parser");

        let parsed = format!("{:?}", code);
        assert_eq!(expect_parsed, parsed, "expect parsed vm");

        let expect_compiled = to_out(
            r#"
            @SP
            D=M
            @END_SP_INIT
            D;JNE
            @256
            D=A
            @SP
            M=D
            (END_SP_INIT)
            //push constant 7
            @7
            D=A
            @SP
            AM=M+1
            A=A-1
            M=D
            //push constant 8
            @8
            D=A
            @SP
            AM=M+1
            A=A-1
            M=D
            //add
            @SP
            AM=M-1
            D=M
            @SP
            AM=M-1
            M=D+M
            @SP
            M=M+1
            (INFINITE_LOOP)
            @INFINITE_LOOP
            0;JMP
            "#,
        );

        let compiled = format!("{}", Bootstrapper::new(vec![code]));
        assert_eq!(expect_compiled, compiled, "expect compiled vm");
    }

    #[test]
    fn u07_stackarithmetic_stacktest() {
        let source = r#"
            // This file is part of www.nand2tetris.org
            // and the book "The Elements of Computing Systems"
            // by Nisan and Schocken, MIT Press.
            // File name: projects/07/StackArithmetic/StackTest/StackTest.vm
            
            // Executes a sequence of arithmetic and logical operations
            // on the stack. 
            push constant 17
            push constant 17
            eq
            push constant 17
            push constant 16
            eq
            push constant 16
            push constant 17
            eq
            push constant 892
            push constant 891
            lt
            push constant 891
            push constant 892
            lt
            push constant 891
            push constant 891
            lt
            push constant 32767
            push constant 32766
            gt
            push constant 32766
            push constant 32767
            gt
            push constant 32766
            push constant 32766
            gt
            push constant 57
            push constant 31
            push constant 53
            add
            push constant 112
            sub
            neg
            and
            push constant 82
            or
            not        
            "#
        .to_owned();

        let expect_parsed = to_out(
            r#"
            push constant 17
            push constant 17
            eq
            push constant 17
            push constant 16
            eq
            push constant 16
            push constant 17
            eq
            push constant 892
            push constant 891
            lt
            push constant 891
            push constant 892
            lt
            push constant 891
            push constant 891
            lt
            push constant 32767
            push constant 32766
            gt
            push constant 32766
            push constant 32767
            gt
            push constant 32766
            push constant 32766
            gt
            push constant 57
            push constant 31
            push constant 53
            add
            push constant 112
            sub
            neg
            and
            push constant 82
            or
            not
            "#,
        );

        let code = VMParsed::parse_new("".to_owned(), source).expect("expect parser");

        let parsed = format!("{:?}", code);
        assert_eq!(expect_parsed, parsed, "expect parsed vm");
    }

    #[test]
    fn u07_memoryaccess_basictest() {
        let source = r#"
            // This file is part of www.nand2tetris.org
            // and the book "The Elements of Computing Systems"
            // by Nisan and Schocken, MIT Press.
            // File name: projects/07/MemoryAccess/BasicTest/BasicTest.vm

            // Executes pop and push commands using the virtual memory segments.
            push constant 10
            pop local 0
            push constant 21
            push constant 22
            pop argument 2
            pop argument 1
            push constant 36
            pop this 6
            push constant 42
            push constant 45
            pop that 5
            pop that 2
            push constant 510
            pop temp 6
            push local 0
            push that 5
            add
            push argument 1
            sub
            push this 6
            push this 6
            add
            sub
            push temp 6
            add
            "#
        .to_owned();

        let expect_parsed = to_out(
            r#"
            push constant 10
            pop local 0
            push constant 21
            push constant 22
            pop argument 2
            pop argument 1
            push constant 36
            pop this 6
            push constant 42
            push constant 45
            pop that 5
            pop that 2
            push constant 510
            pop temp 6
            push local 0
            push that 5
            add
            push argument 1
            sub
            push this 6
            push this 6
            add
            sub
            push temp 6
            add
            "#,
        );

        let code = VMParsed::parse_new("".to_owned(), source).expect("expect parser");

        let parsed = format!("{:?}", code);
        assert_eq!(expect_parsed, parsed, "expect parsed vm");
    }
}
