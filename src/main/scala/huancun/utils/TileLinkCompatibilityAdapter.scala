/** *************************************************************************************
  * Copyright (c) 2020-2021 Institute of Computing Technology, Chinese Academy of Sciences
  * Copyright (c) 2020-2021 Peng Cheng Laboratory
  *
  * XiangShan is licensed under Mulan PSL v2.
  * You can use this software according to the terms and conditions of the Mulan PSL v2.
  * You may obtain a copy of Mulan PSL v2 at:
  *          http://license.coscl.org.cn/MulanPSL2
  *
  * THIS SOFTWARE IS PROVIDED ON AN "AS IS" BASIS, WITHOUT WARRANTIES OF ANY KIND,
  * EITHER EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO NON-INFRINGEMENT,
  * MERCHANTABILITY OR FIT FOR A PARTICULAR PURPOSE.
  *
  * See the Mulan PSL v2 for more details.
  * *************************************************************************************
  */

package huancun.utils

import org.chipsalliance.cde.config.Parameters
import chisel3._
import chisel3.util._
import freechips.rocketchip.diplomacy._
import freechips.rocketchip.tilelink._

class TileLinkCompatibilityAdapter(implicit p: Parameters) extends LazyModule {
  val node = TLAdapterNode()

  lazy val module = new TileLinkCompatibilityAdapterImp(this)
}

class TileLinkCompatibilityAdapterImp(outer: TileLinkCompatibilityAdapter) extends LazyModuleImp(outer) {
  (outer.node.in zip outer.node.out).foreach { case ((in, edgeIn), (out, edgeOut)) =>
    // Forward A channel with compatibility handling
    out.a.valid := in.a.valid
    out.a.bits := in.a.bits
    in.a.ready := out.a.ready

    // Forward B channel with compatibility handling  
    in.b.valid := out.b.valid
    in.b.bits := out.b.bits
    out.b.ready := in.b.ready

    // Forward C channel with compatibility handling
    out.c.valid := in.c.valid
    out.c.bits := in.c.bits
    in.c.ready := out.c.ready

    // Forward D channel with compatibility handling
    in.d.valid := out.d.valid
    in.d.bits := out.d.bits
    out.d.ready := in.d.ready

    // Forward E channel with compatibility handling
    out.e.valid := in.e.valid
    out.e.bits := in.e.bits
    in.e.ready := out.e.ready
  }
}

object TileLinkCompatibilityAdapter {
  def apply()(implicit p: Parameters): TLNode = {
    val adapter = LazyModule(new TileLinkCompatibilityAdapter)
    adapter.node
  }
}
