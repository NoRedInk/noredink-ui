module Assets exposing (Assets, assets)

import Nri.Ui.AssetPath as AssetPath exposing (Asset(Asset))


type alias Assets =
    { activity : String
    , arrowDown : String
    , attention_svg : Asset
    , bulb : String
    , calendar : String
    , checkmark : String
    , class : String
    , clever : String
    , clock : String
    , commentNotStarred_png : Asset
    , commentStarred_png : Asset
    , compass : String
    , darkBlueCheckmark_svg : Asset
    , diagnostic : String
    , document : String
    , download : String
    , edit : String
    , editWriting : String
    , exclamation : String
    , exclamationPoint_svg : Asset
    , facebookBlue_svg : Asset
    , flipper : String
    , footsteps : String
    , gear : String
    , hint_png : Asset
    , iconCalendar_svg : Asset
    , iconCheck_png : Asset
    , iconFlag_png : Asset
    , icons_arrowDownBlue_svg : Asset
    , icons_arrowRightBlue_svg : Asset
    , icons_clockRed_svg : Asset
    , icons_equals_svg : Asset
    , icons_helpBlue_svg : Asset
    , icons_peerReview_svg : Asset
    , icons_plusBlue_svg : Asset
    , icons_quickWrite_svg : Asset
    , icons_searchGray_svg : Asset
    , icons_xBlue_svg : Asset
    , icons_xBlue_svg : Asset
    , key : String
    , leaderboard : String
    , leftArrowBlue_png : Asset
    , level1Badge_png : Asset
    , level2Badge_png : Asset
    , level3Badge_png : Asset
    , lock : String
    , logoRedBlack_svg : Asset
    , newspaper : String
    , openClose : String
    , performance : String
    , personBlue_svg : Asset
    , practice : String
    , premiumLock_svg : Asset
    , preview : String
    , quiz : String
    , seemore : String
    , share : String
    , skip : String
    , smallCheckmark_png : Asset
    , sort : String
    , sortArrow : String
    , speedometer : String
    , squiggly_png : Asset
    , startingOffBadge_png : Asset
    , teach_assignments_copyWhite_svg : Asset
    , twitterBlue_svg : Asset
    , unarchiveBlue2x_png : Asset
    , writingAssignment : String
    , writingcycle : String
    , x : String
    , xWhite_svg : Asset
    , submitting : String
    , rating : String
    , revising : String
    }


assets : Assets
assets =
    { activity = "icon-activity"
    , arrowDown = "icon-arrow-down"
    , attention_svg = Asset "assets/attention.svg"
    , bulb = "icon-bulb"
    , calendar = "icon-calendar"
    , checkmark = "icon-checkmark"
    , class = "icon-class"
    , clever = "icon-clever"
    , clock = "icon-clock"
    , commentNotStarred_png = Asset "assets/comment-notStarred.png"
    , commentStarred_png = Asset "assets/comment-starred.png"
    , compass = "icon-compass"
    , darkBlueCheckmark_svg = Asset "assets/dark-blue-checkmark.svg"
    , diagnostic = "icon-diagnostic"
    , document = "icon-document"
    , download = "icon-download"
    , edit = "icon-edit"
    , editWriting = "icon-edit-writing"
    , exclamation = "icon-exclamation"
    , exclamationPoint_svg = Asset "assets/exclamation-point.svg"
    , facebookBlue_svg = Asset "assets/facebook-blue.svg"
    , flipper = "icon-flipper"
    , footsteps = "icon-footsteps"
    , gear = "icon-gear"
    , hint_png = Asset "assets/hint.png"
    , iconCalendar_svg = Asset "assets/icon-calendar.svg"
    , iconCheck_png = Asset "assets/icon-check.png"
    , iconFlag_png = Asset "assets/icon-flag.png"
    , icons_arrowDownBlue_svg = Asset "assets/arrow-down-blue.svg"
    , icons_arrowRightBlue_svg = Asset "assets/arrow-right-blue.svg"
    , icons_clockRed_svg = Asset "assets/clock-red.svg"
    , icons_equals_svg = Asset "assets/equals.svg"
    , icons_helpBlue_svg = Asset "assets/help-blue.svg"
    , icons_peerReview_svg = Asset "assets/peer-review.svg"
    , icons_plusBlue_svg = Asset "assets/plus-blue.svg"
    , icons_quickWrite_svg = Asset "assets/quick-write.svg"
    , icons_searchGray_svg = Asset "assets/search-gray.svg"
    , icons_xBlue_svg = Asset "assets/x-blue.svg"
    , key = "icon-key"
    , leaderboard = "icon-leaderboard"
    , leftArrowBlue_png = Asset "assets/left-arrow-blue.png"
    , level1Badge_png = Asset "assets/level-1-badge.png"
    , level2Badge_png = Asset "assets/level-2-badge.png"
    , level3Badge_png = Asset "assets/level-3-badge.png"
    , lock = "icon-lock"
    , logoRedBlack_svg = Asset "assets/logo-red-black.svg"
    , newspaper = "icon-newspaper"
    , openClose = "icon-open-close"
    , performance = "icon-performance"
    , personBlue_svg = Asset "assets/person-blue.svg"
    , practice = "icon-practice"
    , premiumLock_svg = Asset "assets/premium-lock.svg"
    , preview = "icon-preview"
    , quiz = "icon-quiz"
    , seemore = "icon-seemore"
    , share = "icon-share"
    , skip = "icon-skip"
    , smallCheckmark_png = Asset "assets/small_checkmark.png"
    , sort = "icon-sort"
    , sortArrow = "icon-sort-arrow"
    , speedometer = "icon-speedometer"
    , squiggly_png = Asset "assets/squiggly.png"
    , startingOffBadge_png = Asset "assets/starting-off-badge.png"
    , teach_assignments_copyWhite_svg = Asset "assets/copy-white.svg"
    , twitterBlue_svg = Asset "assets/twitter-blue.svg"
    , unarchiveBlue2x_png = Asset "assets/unarchive-blue_2x.png"
    , writingAssignment = "icon-writing-assignment"
    , writingcycle = "icon-writingcycle"
    , x = "icon-x"
    , xWhite_svg = Asset "assets/x-white.svg"
    , submitting = "icon-submitting"
    , rating = "icon-rating"
    , revising = "icon-revising"
    }
