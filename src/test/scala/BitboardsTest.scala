import com.github.gekomad.chessgengenerator.board.Bitboards
import org.scalatest.funsuite.AnyFunSuite

class BitboardsTest extends AnyFunSuite {

  test("getDiagonalAntiDiagonal") {
    val a = Bitboards()
    val o = (0 until 64).map { pos =>
      a.getDiagonalAntiDiagonal(pos, 0xffff00000000ffffL)
    }

    val res = List(
      BigInt("512").toLong,
      BigInt("1280").toLong,
      BigInt("2560").toLong,
      BigInt("5120").toLong,
      BigInt("10240").toLong,
      BigInt("20480").toLong,
      BigInt("40960").toLong,
      BigInt("16384").toLong,
      BigInt("9024825867763714").toLong,
      BigInt("18049651735592965").toLong,
      BigInt("36099303487963146").toLong,
      BigInt("141017232965652").toLong,
      BigInt("1659000848424").toLong,
      BigInt("283693466779728").toLong,
      BigInt("567382638592160").toLong,
      BigInt("1134765260406848").toLong,
      BigInt("4512412933816832").toLong,
      BigInt("9024825884411136").toLong,
      BigInt("18049656063789568").toLong,
      BigInt("36100411639206912").toLong,
      BigInt("424704217196544").toLong,
      BigInt("567933457682432").toLong,
      BigInt("1134767403737088").toLong,
      BigInt("2269530512441344").toLong,
      BigInt("2256206450263040").toLong,
      BigInt("4512417195558912").toLong,
      BigInt("9025933902745856").toLong,
      BigInt("18333342782202368").toLong,
      BigInt("36666685564404736").toLong,
      BigInt("1275777090881536").toLong,
      BigInt("2270079204986880").toLong,
      BigInt("4539058881568768").toLong,
      BigInt("1128098963916800").toLong,
      BigInt("2257297456238592").toLong,
      BigInt("4796069889253376").toLong,
      BigInt("9592139778507008").toLong,
      BigInt("19184279557014016").toLong,
      BigInt("38368559113962496").toLong,
      BigInt("4679524173219840").toLong,
      BigInt("9077569074761728").toLong,
      BigInt("562958610993152").toLong,
      BigInt("1407396493664256").toLong,
      BigInt("2814793004105728").toLong,
      BigInt("5629586008276992").toLong,
      BigInt("11259172016488704").toLong,
      BigInt("22518344016200192").toLong,
      BigInt("45036683737433088").toLong,
      BigInt("18014673925310464").toLong,
      BigInt("144117404414255104").toLong,
      BigInt("360293502378065920").toLong,
      BigInt("720587009051099136").toLong,
      BigInt("1441174018118909952").toLong,
      BigInt("2882348036221108224").toLong,
      BigInt("5764696068147249408").toLong,
      BigInt("11529391036782871040").toLong,
      BigInt("4611756524879479808").toLong,
      BigInt("562949953421312").toLong,
      BigInt("1407374883553280").toLong,
      BigInt("2814749767106560").toLong,
      BigInt("5629499534213120").toLong,
      BigInt("11258999068426240").toLong,
      BigInt("22517998136852480").toLong,
      BigInt("45035996273704960").toLong,
      BigInt("18014398509481984").toLong
    )

    assert(o == res)
  }

  test("rankFile") {
    val a = Bitboards()
    val o = (0 until 64).map { pos =>
      a.getRankFile(pos, 0xffff00000000ffffL)
    }

    val res = List(
      BigInt("258").toLong,
      BigInt("517").toLong,
      BigInt("1034").toLong,
      BigInt("2068").toLong,
      BigInt("4136").toLong,
      BigInt("8272").toLong,
      BigInt("16544").toLong,
      BigInt("32832").toLong,
      BigInt("282578800148993").toLong,
      BigInt("565157600298242").toLong,
      BigInt("1130315200596484").toLong,
      BigInt("2260630401192968").toLong,
      BigInt("4521260802385936").toLong,
      BigInt("9042521604771872").toLong,
      BigInt("18085043209543744").toLong,
      BigInt("36170086419021952").toLong,
      BigInt("282578800083200").toLong,
      BigInt("565157600166400").toLong,
      BigInt("1130315200332800").toLong,
      BigInt("2260630400665600").toLong,
      BigInt("4521260801331200").toLong,
      BigInt("9042521602662400").toLong,
      BigInt("18085043205324800").toLong,
      BigInt("36170086410649600").toLong,
      BigInt("282578783371520").toLong,
      BigInt("565157566743040").toLong,
      BigInt("1130315133486080").toLong,
      BigInt("2260630266972160").toLong,
      BigInt("4521260533944320").toLong,
      BigInt("9042521067888640").toLong,
      BigInt("18085042135777280").toLong,
      BigInt("36170084271554560").toLong,
      BigInt("282574505181440").toLong,
      BigInt("565149010362880").toLong,
      BigInt("1130298020725760").toLong,
      BigInt("2260596041451520").toLong,
      BigInt("4521192082903040").toLong,
      BigInt("9042384165806080").toLong,
      BigInt("18084768331612160").toLong,
      BigInt("36169536663224320").toLong,
      BigInt("281479288520960").toLong,
      BigInt("562958577041920").toLong,
      BigInt("1125917154083840").toLong,
      BigInt("2251834308167680").toLong,
      BigInt("4503668616335360").toLong,
      BigInt("9007337232670720").toLong,
      BigInt("18014674465341440").toLong,
      BigInt("36029348930682880").toLong,
      BigInt("72621647814787328").toLong,
      BigInt("145524770606285312").toLong,
      BigInt("291049541212570624").toLong,
      BigInt("582099082425141248").toLong,
      BigInt("1164198164850282496").toLong,
      BigInt("2328396329700564992").toLong,
      BigInt("4656792659401129984").toLong,
      BigInt("9241527724764332032").toLong,
      BigInt("144396663052566528").toLong,
      BigInt("360850920143060992").toLong,
      BigInt("721701840286121984").toLong,
      BigInt("1443403680572243968").toLong,
      BigInt("2886807361144487936").toLong,
      BigInt("5773614722288975872").toLong,
      BigInt("11547229444577951744").toLong,
      BigInt("4647714815446351872").toLong
    )

    assert(o == res)
  }

}
